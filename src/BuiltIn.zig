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

pub fn Execute(allocator: Allocator, kind: Kind, args: []const Object) BuiltInError!Object {
    switch (kind) {
        .len => return try len(args),
        .first => return try first(args),
        .last => return try last(args),
        .push => return try push(allocator, args),
    }
}

fn len(args: []const Object) BuiltInError!Object {
    if (args.len != 1) return error.WrongNumberOfArgs;

    const arg = args[0];

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

fn first(args: []const Object) BuiltInError!Object {
    if (args.len != 1) return error.WrongNumberOfArgs;

    const arg = args[0];

    switch (arg) {
        .array => |array| {
            if (array.elements.len > 0) return array.elements[0];

            return Object{ .nullable = {} };
        },
        else => return error.UnsupportedObjectType,
    }
}

fn last(args: []const Object) BuiltInError!Object {
    if (args.len != 1) return error.WrongNumberOfArgs;

    const arg = args[0];

    switch (arg) {
        .array => |array| {
            const last_idx = array.elements.len;

            if (last_idx > 0) return array.elements[last_idx - 1];

            return Object{ .nullable = {} };
        },
        else => return error.UnsupportedObjectType,
    }
}

fn push(gpa: Allocator, args: []const Object) BuiltInError!Object {
    if (args.len != 2) return error.WrongNumberOfArgs;

    if (args[0] != .array) return error.WrongArgType;

    var new_elements: ArrayList(Object) = .empty;
    errdefer {
        for (new_elements.items) |obj| obj.deinit(gpa);
        new_elements.deinit(gpa);
    }

    try new_elements.ensureUnusedCapacity(gpa, args[0].array.elements.len + 1);
    for (args[0].array.elements) |elem| {
        new_elements.appendAssumeCapacity(try elem.clone(gpa));
    }

    new_elements.appendAssumeCapacity(try args[1].clone(gpa));

    const array_ptr = try gpa.create(object.ArrayObject);
    errdefer gpa.destroy(array_ptr);

    array_ptr.* = object.ArrayObject{
        .elements = try new_elements.toOwnedSlice(gpa),
    };

    return Object{ .array = array_ptr };
}
