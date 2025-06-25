const std = @import("std");
const wasm = @import("wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.FunctionSection);

const ArrayList = std.ArrayListUnmanaged;

const Self = @This();

/// Maps func index to type idx
mapping: ArrayList(u32),

pub const empty = Self{
    .mapping = .empty,
};

pub fn deinit(ctx: *anyopaque, gpa: Allocator) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.mapping.deinit(gpa);
}

pub fn content(ctx: *anyopaque, gpa: Allocator) ![]const u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    var out: ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    const writer = out.writer(gpa);

    var u32_encoder = wasm.LEB128Encoder(u32).init;

    try writer.writeAll(u32_encoder.encode(@intCast(self.mapping.items.len)));

    for (self.mapping.items) |type_idx| {
        try writer.writeAll(u32_encoder.encode(type_idx));
    }

    return out.toOwnedSlice(gpa);
}

pub fn section(self: *Self) Section {
    return Section{
        .ptr = self,
        .vtable = &.{
            .deinit = deinit,
            .content = content,
            .parse = parse,
        },
        .id = .function,
    };
}

pub fn parse(ctx: *anyopaque, gpa: Allocator, bytes: []const u8) !void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    log.debug("decoding len = {}, {x}", .{ bytes.len, bytes });

    var u32_converter = wasm.LEB128Encoder(u32).init;
    const n_functions, var enc_len = try u32_converter.decode(bytes);

    var at = enc_len;

    for (0..n_functions) |_| {
        const type_idx, enc_len = try u32_converter.decode(bytes[at..]);

        try self.mapping.append(gpa, type_idx);

        at += 1;
    }
}
