const std = @import("std");

const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMapUnmanaged;

const object = @import("../object.zig");
const Object = object.Object;

const DictionaryObject = @This();

inner: StringHashMap(Object), // keys are allocated
rc: usize,

pub fn deinit(dictionary_obj: *const DictionaryObject, allocator: Allocator) void {
    var inner_hm = dictionary_obj.inner;

    var iterator = inner_hm.iterator();

    while (iterator.next()) |entry| {
        allocator.free(entry.key_ptr.*);
        entry.value_ptr.deinit(allocator);
    }

    inner_hm.deinit(allocator);

}


pub fn clone(so: *const DictionaryObject, allocator: Allocator) Allocator.Error!Object {
    _ = so;
    _ = allocator;
    @panic("clone for DictionaryObject is unimplemented");
}
