const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("../object.zig");
const Object = object.Object;

const StringObject = @This();

value: []const u8,
rc: usize = 0,

pub fn deinit(so: *const StringObject, allocator: Allocator) void {
    if (so.rc == 0) allocator.free(so.value);
}

pub fn clone(so: *const StringObject, allocator: Allocator) Allocator.Error!Object {

    const new_val = try allocator.alloc(u8, so.value.len);
    errdefer allocator.free(new_val);

    @memcpy(new_val, so.value);

    const str_ptr = try allocator.create(StringObject);
    errdefer allocator.destroy(str_ptr);

    str_ptr.* = StringObject{
        .value = new_val,
    };

    return Object{
        .string = str_ptr
    };

}
