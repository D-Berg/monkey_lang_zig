const std = @import("std");

const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMapUnmanaged;

const object = @import("../object.zig");
const Object = object.Object;

const DictionaryObject = @This();

inner: StringHashMap(Object), // keys are allocated
rc: usize,

// TODO: decrease rc of stored objects
pub fn deinit(dictionary_obj: *const DictionaryObject, allocator: Allocator) void {
    var inner_hm = dictionary_obj.inner;

    var iterator = inner_hm.iterator();

    while (iterator.next()) |entry| {
        allocator.free(entry.key_ptr.*);
        const value_obj = entry.value_ptr.*;
        switch (value_obj) {
            .function => |f| f.rc -= 1,
            .string => |s| s.rc -= 1,
            .array => |a| a.rc -= 1,
            .dictionary => |d| d.rc -= 1,
            else => {},
        }
        value_obj.deinit(allocator);
    }

    inner_hm.deinit(allocator);
}

pub fn format(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("{{", .{});
    var it = self.inner.iterator();
    const len = self.inner.size;
    var i: usize = 0;
    while (it.next()) |entry| : (i += 1) {
        try writer.print("{s}: {f}", .{ entry.key_ptr.*, entry.value_ptr.* });
        if (i + 1 != len) try writer.print(", ", .{});
    }
    try writer.print("}}", .{});
}

pub fn clone(so: *const DictionaryObject, allocator: Allocator) Allocator.Error!Object {
    _ = so;
    _ = allocator;
    @panic("clone for DictionaryObject is unimplemented");
}
