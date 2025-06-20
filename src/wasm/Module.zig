const std = @import("std");
const wasm = @import("wasm.zig");

const Section = wasm.Section;

const EnumArray = std.enums.EnumArray;
const Allocator = std.mem.Allocator;

const Module = @This();

sections: EnumArray(Section.ID, ?Section) = .initFill(null),
type_section: wasm.TypeSection = .empty,
function_section: wasm.FunctionSection = .empty,
export_section: wasm.ExportSection = .empty,
code_section: wasm.CodeSection = .empty,
current_function: *wasm.Function = undefined,

pub const init = Module{};

pub fn deinit(self: *Module, gpa: Allocator) void {
    for (self.sections.values) |maybe_section| {
        if (maybe_section) |section| {
            section.deinit(gpa);
        }
    }
}

pub fn getSection(self: *Module, id: Section.ID) ?Section {
    const i: usize = @intCast(@intFromEnum(id));
    return self.sections.values[i];
}

pub fn addFunction(self: *Module, gpa: Allocator, func: wasm.Function) !void {
    try self.type_section.functions.append(gpa, func);
    try self.code_section.functions.append(gpa, func);
    self.function_section.n_funcs += 1;

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
        else => {},
    }
}

pub fn write(self: *Module, gpa: Allocator, writer: std.io.AnyWriter) !void {
    try writer.writeAll(wasm.MAGIC_MODULE_HEADER[0..]);
    try writer.writeAll(wasm.MODULE_VERSION[0..]);

    var it = self.sections.iterator();

    while (it.next()) |entry| {
        if (entry.value.*) |section| try section.write(gpa, writer);
    }
}
