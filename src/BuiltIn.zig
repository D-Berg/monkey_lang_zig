//! Built in functions for the monkey programming language
const std = @import("std");
const ArrayList = std.ArrayList;
const object = @import("object.zig");
const Object = object.Object;
const eql = std.mem.eql;
pub const BuiltInError = error{ WrongNumberOfArgs, UnsupportedObjectType };

pub const Kind = enum { len, first, last };

pub fn getBuiltInFn(str: []const u8) ?Kind {
    if (eql(u8, "len", str)) return .len;
    if (eql(u8, "first", str)) return .first;
    if (eql(u8, "last", str)) return .last;

    return null;
}

pub fn Execute(kind: Kind, args: *const ArrayList(Object)) BuiltInError!Object {
    switch (kind) {
        .len => return try len(args),
        .first => return try first(args),
        .last => return try last(args),
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
                .integer = @intCast(array.elements.items.len),
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

            if (array.elements.items.len > 0) return array.elements.items[0];
            
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
            const last_idx = array.elements.items.len;

            if (array.elements.items.len > 0) return array.elements.items[last_idx - 1];
            
            return Object {.nullable = {}};
        },
        else => return error.UnsupportedObjectType,



    }

}
