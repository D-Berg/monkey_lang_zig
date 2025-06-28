const std = @import("std");
const wasm = @import("../wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.CodeSection);

const ArrayList = std.ArrayListUnmanaged;

const Self = @This();

functions: ArrayList(wasm.Function),

pub const empty = Self{ .functions = .empty };

pub fn deinit(ctx: *anyopaque, gpa: Allocator) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.functions.deinit(gpa);
}

pub fn content(ctx: *anyopaque, gpa: Allocator) ![]const u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    var out: ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    const writer = out.writer(gpa);

    var u32_encoder = wasm.LEB128Encoder(u32).init;

    const n_funcs: u32 = @intCast(self.functions.items.len);

    try writer.writeAll(u32_encoder.encode(n_funcs));

    for (self.functions.items) |func| {
        var function_section: ArrayList(u8) = .empty;
        defer function_section.deinit(gpa);

        try function_section.appendSlice(gpa, &.{ 1, 1, 0x7f }); // locals
        try function_section.appendSlice(gpa, @ptrCast(func.body.items));

        try writer.writeAll(u32_encoder.encode(@intCast(function_section.items.len)));
        try writer.writeAll(function_section.items);
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
        .id = .code,
    };
}

pub fn parse(ctx: *anyopaque, gpa: Allocator, bytes: []const u8) !void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    _ = self;
    _ = gpa;
    log.debug("decoding len = {}, {x}", .{ bytes.len, bytes });
}
