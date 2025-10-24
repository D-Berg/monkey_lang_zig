const std = @import("std");
const HashMap = @import("hash_map.zig").HashMap;
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const trace = @import("tracy.zig").trace;

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
        const tracy = trace(@src());
        defer tracy.end();

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

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .nullable => if (@import("builtin").mode == .Debug) try writer.print("null", .{}),
            inline .integer, .boolean => |val| try writer.print("{}", .{val}),
            inline .built_in => |built_in| try writer.print("{t}", .{built_in}),
            inline else => |case| {
                try writer.print("{f}", .{case});
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

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("[", .{});
        for (self.elements, 0..) |e, i| {
            try writer.print("{f}", .{e});
            if (i + 1 != self.elements.len) try writer.print(", ", .{});
        }
        try writer.print("]", .{});
    }
};
