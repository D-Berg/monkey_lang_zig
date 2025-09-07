const std = @import("std");
const HashMap = @import("hash_map.zig").HashMap;
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");

pub const FunctionObject = @import("objects/FunctionObject.zig");
pub const ReturnObject = @import("objects/ReturnObject.zig");
pub const StringObject = @import("objects/StringObject.zig");
pub const DictionayObject = @import("objects/DictionaryObject.zig");

const BuiltIn = @import("BuiltIn.zig");

const ArrayList = std.ArrayListUnmanaged;
const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;
const print = std.debug.print;
const log = std.log.scoped(.Object);

const Allocator = std.mem.Allocator;

pub const Object = union(enum) {
    pub const ID = enum(u32) { _ };
    integer: i64,
    boolean: bool,
    nullable,
    return_val_obj: ReturnObject,
    function: *FunctionObject,
    string: *StringObject,
    built_in: BuiltIn.Kind,
    array: *ArrayObject,
    dictionary: *DictionayObject,

    // maybe frees memory depending on rc
    pub fn deinit(obj: *const Object, allocator: Allocator) void {

        // print("Deinitalizing object\n", .{});
        switch (obj.*) {
            .return_val_obj => |rvj| {
                log.debug("Deinitalizing return object\n", .{});
                rvj.deinit(allocator);
            },
            .function => |func| {
                // log.debug("func.rc = {}", .{func.rc});
                log.debug("trying to deinitalizing func object\n", .{});
                // is only deinited if func_obj dont have a owner
                if (func.rc == 0) {
                    func.deinit(allocator);
                    log.debug("allocator: destroying {*}", .{func});
                    allocator.destroy(func);
                } else {
                    log.debug("did not deinit func {*}, cause its referenced by {} other\n", .{ func, func.rc });
                }
            },

            .string => |string| {
                if (string.rc == 0) {
                    string.deinit(allocator);
                    log.debug("allocator: destroying {*}", .{string});
                    allocator.destroy(string);
                }
            },

            .array => |array| {
                if (array.rc == 0) {
                    array.deinit(allocator);
                    allocator.destroy(array);
                }
            },

            .dictionary => |dict| {
                if (dict.rc == 0) {
                    dict.deinit(allocator);
                    allocator.destroy(dict);
                }
            },

            else => {},
        }
    }

    pub fn destroy(obj: *const Object, allocator: Allocator) void {
        switch (obj.*) {
            .array => |array| array.rc = 0,
            .string => |string| string.rc = 0,
            .function => |function| function.rc = 0,
            else => {},
        }

        obj.deinit(allocator);
    }

    pub fn clone(obj: *const Object, allocator: Allocator) !Object {
        switch (obj.*) {
            .integer, .boolean, .nullable, .built_in => {
                return obj.*;
            },
            .function => {
                @panic("clone for FunctionObject not yet implemented");
            },
            inline else => |case| {
                return case.clone(allocator);
            },
        }
    }

    /// return string of value, str need to be deallocated by caller
    pub fn inspect(obj: *const Object, out: *std.Io.Writer) !void {
        switch (obj.*) {
            .nullable => {
                try out.print("null", .{});
            },
            .string => |so| {
                try out.print("{s}", .{so.value});
            },
            .array => |array| {
                try out.print("{s}", .{"["});

                const n_elems = array.elements.len;

                for (array.elements, 0..) |elem, i| {
                    try elem.inspect(out);

                    if (i != n_elems - 1) try out.print(", ", .{});
                }

                try out.print("]", .{});
            },
            .function => |func| {
                try out.print("fn ( ", .{});

                const n_params = func.params.len;
                for (func.params, 0..) |param, i| {
                    try out.print("{s}", .{param.token.literal});

                    if (i + 1 < n_params) {
                        try out.print(", ", .{});
                    } else {
                        try out.print(" ", .{});
                    }
                }

                try out.print("{s}", .{") {"});

                // TODO: fix no allocation
                // const body_str = try func.body.String(allocator);
                // defer allocator.free(body_str);
                //
                // try string.appendSlice(allocator, body_str);
                //
                // try string.appendSlice(allocator, " }");
                //
                // return try string.toOwnedSlice(allocator);
            },
            .dictionary => |dict| {
                const n_entries = dict.inner.size;
                var iterator = dict.inner.iterator();

                try out.print("{s}", .{"{ "});

                var i: usize = 0;
                while (iterator.next()) |entry| : (i += 1) {
                    try out.print("{s}: ", .{entry.key_ptr.*});
                    try entry.value_ptr.inspect(out);

                    if (i + 1 == n_entries) {
                        try out.print("{s}", .{" }"});
                    } else {
                        try out.print(", ", .{});
                    }
                }
            },
            inline else => |case| {
                try out.print("{}", .{case});
            },
        }
    }
};

pub const ArrayObject = struct {
    elements: []const Object,
    rc: usize = 0,

    pub fn deinit(array_object: *const ArrayObject, allocator: Allocator) void {
        if (array_object.rc == 0) {
            for (array_object.elements) |elem| {
                elem.deinit(allocator);
            }
            allocator.free(array_object.elements);
        }
    }

    pub fn clone(ao: *const ArrayObject, allocator: Allocator) Allocator.Error!Object {
        _ = allocator;
        _ = ao;
        @panic("clone for ArrayObject not implemented");
    }
};
