const std = @import("std");
const wasm = @import("wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.TypeSection);

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
        const p_len: u32 = @intCast(func.param_types.len);
        const r_len: u32 = @intCast(func.return_types.len);

        try writer.writeByte(wasm.ValType.function.byte());

        try writer.writeAll(u32_encoder.encode(p_len));
        try writer.writeAll(@ptrCast(func.param_types));

        try writer.writeAll(u32_encoder.encode(r_len));
        try writer.writeAll(@ptrCast(func.return_types));
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
        .id = .type,
    };
}

pub fn parse(ctx: *anyopaque, gpa: Allocator, bytes: []const u8) !void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    _ = self;
    _ = gpa;
    log.debug("decoding, len = {}, {x}", .{ bytes.len, bytes });
}
