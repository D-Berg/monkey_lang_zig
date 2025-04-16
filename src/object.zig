const std = @import("std");
const HashMap = @import("hash_map.zig").HashMap;
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const BuiltIn = @import("BuiltIn.zig");

const ArrayList = std.ArrayList;
const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;
const print = std.debug.print;
const log = std.log;

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

    pub fn deinit(obj: *const Object, allocator: Allocator) void {

        // print("Deinitalizing object\n", .{});
        switch (obj.*) {
            .return_val_obj => |rvj| {
                log.debug("Deinitalizing return object\n", .{});
                rvj.deinit(allocator);
            },
            .function => |func| {
                log.debug("trying to deinitalizing func object\n", .{});
                // is only deinited if func_obj dont have a owner
                if (func.rc == 0) {
                    func.deinit(allocator);
                    allocator.destroy(func);
                } else {
                    log.debug("did not deinit func {*}, cause its referenced by {} other\n", .{ func, func.rc });
                }
            },

            .string => |so| {
                if (so.rc == 0) {
                    so.deintit(allocator);
                    allocator.destroy(so);
                }
            },

            .array => |array| {
                if (array.rc == 0) {
                    array.deinit(allocator);
                    allocator.destroy(array);
                }
            },

            else => {},
        }
    }

    pub fn clone(obj: *const Object) !Object {
        switch (obj.*) {
            .integer, .boolean, .nullable, .built_in => {
                return obj.*;
            },
            .function => {
                @panic("clone for FunctionObject not yet implemented");
            },
            inline else => |case| {
                return case.clone();
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

                const n_elems = array.elements.items.len;

                for (array.elements.items, 0..) |elem, i| {

                    const elem_str = try elem.inspect(allocator);
                    defer allocator.free(elem_str);

                    try array_str.appendSlice(elem_str);
                    

                    if (i != n_elems - 1) try array_str.appendSlice(", ");

                }
                
                try array_str.append(']');

                return array_str.toOwnedSlice();

            },
            inline else => |case| {
                const str = try std.fmt.allocPrint(allocator, "{}", .{case});
                return str;
            },
        }
    }
};

const ReturnObject = struct {
    value: *const Object,
    owner: ?*Environment,

    pub fn deinit(ret_obj: *const ReturnObject, allocator: Allocator) void {
        // print("deinits ret obj\n", .{});
        ret_obj.value.deinit(allocator);
        allocator.destroy(ret_obj.value);
    }

    pub fn clone(ro: *const ReturnObject) Allocator.Error!Object {
        const value_ptr = try ro.allocator.create(Object);
        value_ptr.* = try ro.value.clone();

        return Object{ .return_val_obj = .{
            .allocator = ro.allocator,
            .value = value_ptr,
            .owner = ro.owner,
        } };
    }
};

pub const FunctionObject = struct {
    params: ArrayList(Identifier),
    body: BlockStatement,
    env: *Environment,
    rc: usize = 0, // the env that owns the object have the responsebility to destroy it, if null it should deallocate

    pub fn deinit(fnc_obj: *const FunctionObject, _: Allocator) void {

        // TODO: only deinit if obj dont have a owner

        log.debug("trying to deinit fn obj, addr: {*}\n", .{fnc_obj});

        if (fnc_obj.rc != 0) {
            log.debug("dont deinits fnc_obj {*} since its referenced by {} other objects\n", .{ fnc_obj, fnc_obj.rc });
        } else {

            // Only deinit if fnc_obj dont have a owner

            log.debug("func_obj ref count is {} , deinits\n", .{fnc_obj.rc});

            fnc_obj.body.deinit();

            for (fnc_obj.params.items) |p| {
                p.deinit();
            }
            fnc_obj.params.deinit();

            if (fnc_obj.env.outer) |_| {
                log.debug("deinits func objects enclosed env: {*}\n", .{fnc_obj.env});
                fnc_obj.env.rc -= 1;
                fnc_obj.env.deinit();
                // fnc_obj.allocator.destroy(fnc_obj.env);

            } else {
                log.debug("func env.outer = null. dont deinits its env\n", .{});
                // since its the main env

            }
        }

        //
    }

    // pub fn clone(fo: *const FunctionObject) Allocator.Error!Object {
    //
    //     print("cloning func {*}\n", .{fo});
    //
    //     var params = ArrayList(Identifier).init(fo.params.allocator);
    //
    //     for (fo.params.items) |p| {
    //
    //         try params.append(try p.clone());
    //
    //     }
    //
    //     const body = try fo.body.clone();
    //
    //     // var new_env: *Environment = undefined;
    //     //
    //     // // expensive way of doing, but as of
    //     // // right now fn objects gets cloned and deinited
    //     // // at each fn call and if it has an enclosed env,
    //     // // the enclosed env get deinited asweell
    //     // // TODO: come up with a better wwayy future me
    //     // if (fo.env.outer != null) {
    //     //     // enclosed env
    //     //     new_env = try fo.env.clone();
    //     //     new_env.outer = fo.env.outer;
    //     //
    //     // } else {
    //     //     // fo.env is the main env
    //     //     new_env = fo.env;
    //     // }
    //     //
    //     return Object {
    //         .function = .{
    //             .allocator = fo.allocator,
    //             .params = params,
    //             .body = body,
    //             .env = fo.env,
    //             .owner = fo.owner,
    //         }
    //     };
    //
    // }

    /// For debuging
    /// Caller need to deinit returned str
    pub fn String(fo: *const FunctionObject) Allocator.Error![]const u8 {
        const n_params = fo.params.items.len;

        if (fo.env.outer) |outer| {
            return try std.fmt.allocPrint(fo.allocator, "fn object: p: {}, env: {*}, outer_env: {*}", .{ n_params, fo.env, outer });
        }
        return try std.fmt.allocPrint(fo.allocator, "fn object: p: {}, env: {*}, outer_env: {?}", .{ n_params, fo.env, fo.env.outer });
    }
};

pub const HashMapObject = struct {
    inner: std.StringHashMap(Object),
    rc: usize = 0,
};

pub const StringObject = struct {
    allocator: Allocator,
    value: []const u8,
    rc: usize = 0,

    pub fn deintit(so: *const StringObject) void {
        if (so.rc == 0) so.allocator.free(so.value);
    }

    pub fn clone(so: *const StringObject) Allocator.Error!Object {

        const new_val = try so.allocator.alloc(u8, so.value.len);
        @memcpy(new_val, so.value);

        const so_ptr = try so.allocator.create(StringObject);

        so_ptr.* = StringObject{
            .allocator = so.allocator,
            .value = new_val,
        };

        return Object{
            .string = so_ptr

        };

    }
};

pub const ArrayObject = struct {
    allocator: Allocator,
    elements: ArrayList(Object),
    rc: usize = 0,

    pub fn deinit(ao: *const ArrayObject) void {
        if (ao.rc == 0) {
            for (ao.elements.items) |elem| {
                elem.deinit();
            }
            ao.elements.deinit();
        }

    }

    pub fn clone(ao: *const ArrayObject) Allocator.Error!Object {
        _ = ao;
        @panic("clone for ArrayObject not implemented");

    }

};

//
