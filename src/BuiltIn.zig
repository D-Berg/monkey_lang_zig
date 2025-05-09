//! Built in functions for the monkey programming language
const std = @import("std");
const ArrayList = std.ArrayList;
const object = @import("object.zig");
const Object = object.Object;
const eql = std.mem.eql;
const Allocator = std.mem.Allocator;
pub const BuiltInError = error{ WrongNumberOfArgs, UnsupportedObjectType, WrongArgType } || Allocator.Error;

pub const Kind = enum { len, first, last, push };

pub fn getBuiltInFn(str: []const u8) ?Kind {
    if (eql(u8, "len", str)) return .len;
    if (eql(u8, "first", str)) return .first;
    if (eql(u8, "last", str)) return .last;
    if (eql(u8, "push", str)) return .push;

    return null;
}

pub fn Execute(allocator: Allocator, kind: Kind, args: *const ArrayList(Object)) BuiltInError!Object {
    switch (kind) {
        .len => return try len(args),
        .first => return try first(args),
        .last => return try last(args),
        .push => return try push(allocator, args),
    }
}



fn len(args: *const ArrayList(Object)) BuiltInError!Object {

    if (args.items.len != 1) return error.WrongNumberOfArgs;

    const arg = args.items[0];

    switch (arg) {
        .string => |str| {
            return Object{
                .integer = @intCast(str.value.len),
            };
        },

        .array => |array| {
            return Object{
                .integer = @intCast(array.elements.len),
            };
        },

        else => return error.UnsupportedObjectType,
    }
}

fn first(args: *const ArrayList(Object)) BuiltInError!Object {

    if (args.items.len != 1) return error.WrongNumberOfArgs;

    const arg = args.items[0];

    switch (arg) {
        .array => |array| {

            if (array.elements.len > 0) return array.elements[0];
            
            return Object {.nullable = {}};

        },
        else => return error.UnsupportedObjectType,

    }
}

fn last(args: *const ArrayList(Object)) BuiltInError!Object {

    if (args.items.len != 1) return error.WrongNumberOfArgs;

    const arg = args.items[0];

    switch (arg) {
            
        .array => |array| {
            const last_idx = array.elements.len;

            if (last_idx > 0) return array.elements[last_idx - 1];
            
            return Object {.nullable = {}};
        },
        else => return error.UnsupportedObjectType,



    }
}

fn push(allocator: Allocator, args: *const ArrayList(Object)) BuiltInError!Object {

    if (args.items.len != 2) return error.WrongNumberOfArgs;

    const arg1 = args.items[0];
    const arg2 = args.items[1];

    if (arg1 != .array) return error.WrongArgType;

    var new_elements = ArrayList(Object).init(allocator);
    errdefer {
        for (new_elements.items) |obj| obj.deinit(allocator);
        new_elements.deinit();
    }

    for (arg1.array.elements) |elem| {
        try new_elements.append(try elem.clone(allocator));
    }

    try new_elements.append(try arg2.clone(allocator));
 
    const new_array_ptr = try allocator.create(object.ArrayObject);
    errdefer allocator.destroy(new_array_ptr);

    new_array_ptr.* = object.ArrayObject {
        .elements = try new_elements.toOwnedSlice(),
    };

    return Object{
        .array = new_array_ptr
    };

}
