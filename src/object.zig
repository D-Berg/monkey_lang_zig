const std = @import("std");
const HashMap = @import("hash_map.zig").HashMap;
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");

pub const FunctionObject = @import("objects/FunctionObject.zig");
pub const ReturnObject = @import("objects/ReturnObject.zig");
pub const StringObject = @import("objects/StringObject.zig");
pub const DictionayObject = @import("objects/DictionaryObject.zig");


const BuiltIn = @import("BuiltIn.zig");

const ArrayList = std.ArrayList;
const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;
const print = std.debug.print;
const log = std.log.scoped(.@"Object");

const Allocator = std.mem.Allocator;

pub const Object = union(enum) {
    integer: i32,
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
            else => {}
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
    pub fn inspect(obj: *const Object, allocator: Allocator) Allocator.Error![]const u8 {
        switch (obj.*) {
            .nullable => {
                const str = try std.fmt.allocPrint(allocator, "null", .{});
                return str;
            },
            .string => |so| {
                const str = try std.fmt.allocPrint(allocator, "{s}", .{so.value});
                return str;
            },
            .array => |array| {
                
                var array_str: ArrayList(u8) = .init(allocator);
                errdefer array_str.deinit();
            
                try array_str.append('[');

                const n_elems = array.elements.len;

                for (array.elements, 0..) |elem, i| {

                    const elem_str = try elem.inspect(allocator);
                    defer allocator.free(elem_str);

                    try array_str.appendSlice(elem_str);

                    if (i != n_elems - 1) try array_str.appendSlice(", ");

                }
                
                try array_str.append(']');

                return array_str.toOwnedSlice();

            },
            .dictionary => |dict| {
                var dict_str: ArrayList(u8) = .init(allocator);
                errdefer dict_str.deinit();

                const writer = dict_str.writer();

                const n_entries = dict.inner.size;
                var iterator = dict.inner.iterator();

                try writer.print("{s}", .{"{ "});

                var i: u32 = 0;
                var extra = ", ";
                while (iterator.next()) |entry| : (i += 1){
                    const val_str = try entry.value_ptr.inspect(allocator);
                    defer allocator.free(val_str);

                    if (i + 1 == n_entries) extra = " }";

                    try writer.print("{s}: {s}{s}", .{ 
                        entry.key_ptr.*, val_str, extra
                    }); 
                } 

                return try dict_str.toOwnedSlice();
            },
            inline else => |case| {
                const str = try std.fmt.allocPrint(allocator, "{}", .{case});
                return str;
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

