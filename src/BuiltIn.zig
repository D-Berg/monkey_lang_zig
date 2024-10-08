//! Built in functions for the monkey programming language
const std = @import("std");
const ArrayList = std.ArrayList;
const object = @import("object.zig");
const Object = object.Object;

pub const BuiltInError = error {
    WrongNumberOfArgs,
    UnsupportedObjectType
};

pub const Kind = enum {
    len,
};

pub fn len(args: *const ArrayList(Object)) BuiltInError!Object {

    if (args.items.len != 1) return error.WrongNumberOfArgs;
    

    const arg = args.items[0];

    switch (arg) {
        .string => |str| {

            return Object {
                .integer = @intCast(str.value.len),
            };

        },

        else => return error.UnsupportedObjectType
    }

}
