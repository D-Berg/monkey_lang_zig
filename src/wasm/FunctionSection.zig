const std = @import("std");
const wasm = @import("wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;

const ArrayList = std.ArrayListUnmanaged;

const Self = @This();

n_funcs: u32,

pub const empty = Self{
    .n_funcs = 0,
};

pub fn deinit(ctx: *anyopaque, gpa: Allocator) void {
    _ = ctx;
    _ = gpa;
}

pub fn content(ctx: *anyopaque, gpa: Allocator) ![]const u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    var out: ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    const writer = out.writer(gpa);

    var u32_encoder = wasm.LEB128Encoder(u32).init;

    try writer.writeAll(u32_encoder.encode(self.n_funcs));

    var i: u32 = 0;
    while (i < self.n_funcs) : (i += 1) {
        try writer.writeAll(u32_encoder.encode(i));
    }

    return out.toOwnedSlice(gpa);
}

pub fn section(self: *Self) Section {
    return Section{
        .ptr = self,
        .vtable = &.{
            .deinit = deinit,
            .content = content,
        },
        .id = .function,
    };
}
