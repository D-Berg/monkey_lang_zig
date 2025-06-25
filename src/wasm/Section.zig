//! Interface for the different sections
const std = @import("std");
const wasm = @import("wasm.zig");
// https://webassembly.github.io/spec/core/binary/modules.html#binary-typeidx
const Section = @This();
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.Section);

pub const ID = enum(u8) {
    custom = 0x00,
    type = 0x01,
    import = 0x02,
    function = 0x03,
    table = 0x04,
    memory = 0x05,
    global = 0x06,
    @"export" = 0x07,
    start = 0x08,
    element = 0x09,
    code = 0x0a,
    data = 0x0b,
    data_count = 0x0c,

    pub inline fn byte(self: Section.ID) u8 {
        return @intFromEnum(self);
    }
};

ptr: *anyopaque,
vtable: *const VTable,
id: ID, // TODO: dont store id, get it from idx instead in module

const VTable = struct {
    deinit: *const fn (*anyopaque, gpa: Allocator) void,
    content: *const fn (*anyopaque, gpa: Allocator) anyerror![]const u8,
    parse: *const fn (*anyopaque, gpa: Allocator, bytes: []const u8) anyerror!void,
};

pub fn deinit(self: Section, gpa: Allocator) void {
    self.vtable.deinit(self.ptr, gpa);
}

pub fn write(
    self: Section,
    gpa: Allocator,
    writer: std.io.AnyWriter,
) !void {
    log.debug("writing {s} section: 0x{x}", .{ @tagName(self.id), self.id.byte() });
    try writer.writeByte(self.id.byte());

    const content = try self.vtable.content(self.ptr, gpa);
    defer gpa.free(content);

    var encoder = wasm.LEB128Encoder(u32).init;

    try writer.writeAll(encoder.encode(@intCast(content.len)));

    try writer.writeAll(content);
}

pub fn parse(self: Section, gpa: Allocator, bytes: []const u8) !void {
    try self.vtable.parse(self.ptr, gpa, bytes);
}
