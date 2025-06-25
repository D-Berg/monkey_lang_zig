const std = @import("std");
const wasm = @import("wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.ExportSection);

const ArrayList = std.ArrayListUnmanaged;
const StringHashMap = std.StringHashMapUnmanaged;

const Self = @This();

/// key mem is not allocated
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
            .parse = parse,
        },
        .id = .@"export",
    };
}

pub fn parse(ctx: *anyopaque, gpa: Allocator, bytes: []const u8) !void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    log.debug("decoding len = {}, {x}", .{ bytes.len, bytes });

    var u32_converter = wasm.LEB128Encoder(u32).init;

    const n_exports, var enc_len = try u32_converter.decode(bytes);
    var at = enc_len;

    for (0..n_exports) |_| {
        const name_len, enc_len = try u32_converter.decode(bytes[at..]);
        at += enc_len;

        const name = bytes[at..(at + name_len)];
        log.debug("found export {s}", .{name});
        at += name_len;

        const kind: wasm.ExportDescription.Kind = @enumFromInt(bytes[at]);
        log.debug("kind = {s}", .{@tagName(kind)});
        at += 1;

        const idx, enc_len = try u32_converter.decode(bytes[at..]);
        at += enc_len;

        log.debug("idx = {}", .{idx});

        // NOTE: name is only valid as long as bytes are (wasm input program)
        try self.exports.put(gpa, name, .{ .idx = idx, .kind = kind });
    }
}
