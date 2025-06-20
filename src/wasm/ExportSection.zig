const std = @import("std");
const wasm = @import("wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;

const ArrayList = std.ArrayListUnmanaged;
const StringHashMap = std.StringHashMapUnmanaged;

const Self = @This();

exports: StringHashMap(wasm.ExportDescription),

pub const empty = Self{ .exports = .empty };

pub fn deinit(ctx: *anyopaque, gpa: Allocator) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.exports.deinit(gpa);
}

/// n_exports vector(name_len name export_desc)
pub fn content(ctx: *anyopaque, gpa: Allocator) ![]const u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    var out: ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    const writer = out.writer(gpa);

    var u32_encoder = wasm.LEB128Encoder(u32).init;

    const n_exports: u32 = @intCast(self.exports.count());

    try writer.writeAll(u32_encoder.encode(n_exports));

    var it = self.exports.iterator();

    while (it.next()) |entry| {
        const name_len: u32 = @intCast(entry.key_ptr.len);
        try writer.writeAll(u32_encoder.encode(name_len));
        try writer.writeAll(entry.key_ptr.*);

        try writer.writeByte(@intFromEnum(entry.value_ptr.kind));
        try writer.writeAll(u32_encoder.encode(entry.value_ptr.idx));
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
        .id = .@"export",
    };
}
