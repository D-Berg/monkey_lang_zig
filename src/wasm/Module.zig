const std = @import("std");
const wasm = @import("../wasm.zig");
const log = std.log.scoped(.Module);

const Section = wasm.Section;

const EnumArray = std.enums.EnumArray;
const StringArrayHashMap = std.StringArrayHashMapUnmanaged;
const Allocator = std.mem.Allocator;

const Module = @This();

const runtime = @import("runtime").bytes;

sections: EnumArray(Section.ID, ?Section) = .initFill(null),
type_section: wasm.TypeSection = .empty,
function_section: wasm.FunctionSection = .empty,
export_section: wasm.ExportSection = .empty,
code_section: wasm.CodeSection = .empty,
current_function: *wasm.Function = undefined,

globals: StringArrayHashMap(Global),

pub const Global = struct {
    global_type: GlobalType,
    init_val: InitValue,

    const InitValue = union(wasm.ValType) {
        i32: i32,
        i64: i64,
        f32: f32,
        f64: f32,
        funcref: u32,
        externref: u32,
        function: u32,
        result: u32,
    };

    pub const GlobalType = struct {
        val_type: wasm.ValType,
        mut: Mutability,

        pub const Mutability = enum(u8) {
            @"const" = 0x00,
            @"var" = 0x01,
        };
    };
};

pub const init = Module{ .globals = .empty };

pub fn deinit(self: *Module, gpa: Allocator) void {
    for (self.sections.values) |maybe_section| {
        if (maybe_section) |section| {
            section.deinit(gpa);
        }
    }

    self.globals.deinit(gpa);
}

pub fn getSection(self: *Module, id: Section.ID) ?Section {
    const i: usize = @intCast(@intFromEnum(id));
    return self.sections.values[i];
}

pub fn addFunction(self: *Module, gpa: Allocator, func: wasm.Function) !void {
    const type_func = try wasm.TypeSection.Function.init(
        gpa,
        func.param_types,
        func.return_types,
    );
    try self.type_section.functions.append(gpa, type_func);
    try self.code_section.functions.append(gpa, func);

    const n_funcs = self.type_section.functions.items.len;
    std.debug.assert(n_funcs > 0);
    const type_idx = self.type_section.functions.items.len - 1;
    try self.function_section.mapping.append(gpa, @intCast(type_idx));

    if (func.@"export") {
        try self.export_section.exports.put(gpa, func.name, .{ .kind = .function, .idx = 0 });
    }
}

pub fn addSection(self: *Module, id: Section.ID) void {
    const i: usize = @intCast(@intFromEnum(id));

    switch (id) {
        .type => self.sections.values[i] = self.type_section.section(),
        .function => self.sections.values[i] = self.function_section.section(),
        .@"export" => self.sections.values[i] = self.export_section.section(),
        .code => self.sections.values[i] = self.code_section.section(),
        else => {
            std.debug.panic("{s} section not implemented yet", .{@tagName(id)});
        },
    }
}

// pub fn getGlobalByName(name: []const u8) ?*

pub fn write(self: *Module, gpa: Allocator, writer: std.io.AnyWriter) !void {
    try writer.writeAll(wasm.MAGIC_MODULE_HEADER[0..]);
    try writer.writeAll(wasm.MODULE_VERSION[0..]);

    var it = self.sections.iterator();

    while (it.next()) |entry| {
        if (entry.value.*) |section| try section.write(gpa, writer);
    }
}

pub fn parse(self: *Module, gpa: Allocator, bytes: []const u8) !void {
    var at: usize = 0;

    if (!std.mem.eql(u8, bytes[at..4], &wasm.MAGIC_MODULE_HEADER)) {
        return error.InvalidMagicHeader;
    }

    at += 4;

    if (!std.mem.eql(u8, bytes[at..(at + 4)], &wasm.MODULE_VERSION)) {
        return error.UnsupportedVersion;
    }

    at += 4;

    // TODO: parallel
    sw: switch (bytes[at]) {
        Section.ID.custom.byte(),
        Section.ID.type.byte(),
        Section.ID.import.byte(),
        Section.ID.function.byte(),
        Section.ID.table.byte(),
        Section.ID.memory.byte(),
        Section.ID.global.byte(),
        Section.ID.@"export".byte(),
        Section.ID.start.byte(),
        Section.ID.element.byte(),
        Section.ID.code.byte(),
        Section.ID.data.byte(),
        Section.ID.data_count.byte(),
        => |id| {
            const sec_id: Section.ID = @enumFromInt(id);
            const section_bytes = try sliceSection(&at, bytes);

            self.addSection(sec_id);

            if (self.getSection(sec_id)) |sec| try sec.parse(gpa, section_bytes);

            if (at < bytes.len) {
                continue :sw bytes[at];
            } else {
                break :sw;
            }
        },
        else => |b| {
            log.debug("unknown section id 0x{x}", .{b});
            return error.InvalidSectionId;
        },
    }
}

fn sliceSection(at: *usize, bytes: []const u8) ![]const u8 {
    var u32_converter = wasm.LEB128Encoder(u32).init;

    at.* += 1;

    const section_len, const enc_len = try u32_converter.decode(bytes[at.*..]);

    at.* += enc_len;

    const section = bytes[at.*..(at.* + @as(usize, @intCast(section_len)))];

    at.* += section.len;

    return section;
}

// test "parse bool.wasm" {
//     std.testing.log_level = .debug;
//     // (module
//     //   (type (;0;) (func (result i32)))
//     //   (func (;0;) (type 0) (result i32)
//     //     (local i32)
//     //     i32.const 3
//     //     i32.const 3
//     //     i32.eq
//     //     return)
//     //   (export "__monkey_main" (func 0)))
//     const byte_code = [_]u8{
//         0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f, 0x03,
//         0x02, 0x01, 0x00, 0x07, 0x11, 0x01, 0x0d, 0x5f, 0x5f, 0x6d, 0x6f, 0x6e, 0x6b, 0x65, 0x79, 0x5f,
//         0x6d, 0x61, 0x69, 0x6e, 0x00, 0x00, 0x0a, 0x0c, 0x01, 0x0a, 0x01, 0x01, 0x7f, 0x41, 0x03, 0x41,
//         0x03, 0x46, 0x0f, 0x0b,
//     };
//
//     const gpa = std.testing.allocator;
//     var module = Module.init;
//     defer module.deinit(gpa);
//
//     try module.parse(gpa, &byte_code);
//
//     var expected = std.ArrayListUnmanaged(u8).empty;
//     defer expected.deinit(gpa);
//
//     try module.write(gpa, expected.writer(gpa).any());
//
//     try std.testing.expectEqualSlices(u8, &byte_code, expected.items);
// }

// see src/wasm_runtime.zig
// test "parse runtime" {
//     std.testing.log_level = .err;
//
//     const gpa = std.testing.allocator;
//     var module = Module.init;
//     defer module.deinit(gpa);
//
//     try module.parse(gpa, runtime);
// }

test "runtime magic" {
    if (!std.mem.eql(u8, runtime[0..4], &wasm.MAGIC_MODULE_HEADER)) {
        return error.InvalidMagicHeader;
    }
}
