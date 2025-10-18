const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("../object.zig");
const Object = object.Object;

const Environment = @import("../Environment.zig");

const ReturnObject = @This();

value: *const Object,
owner: ?*Environment,

pub fn deinit(ret_obj: *const ReturnObject, allocator: Allocator) void {
    // print("deinits ret obj\n", .{});
    ret_obj.value.deinit(allocator);
    allocator.destroy(ret_obj.value);
}

pub fn clone(ro: *const ReturnObject, allocator: Allocator) Allocator.Error!Object {
    const value_ptr = try allocator.create(Object);
    errdefer allocator.destroy(value_ptr);

    value_ptr.* = try ro.value.clone(allocator);

    return Object{ .return_val_obj = .{
        .value = value_ptr,
        .owner = ro.owner,
    } };
}

pub fn format(
    self: ReturnObject,
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("{f}", .{self.value});
}
