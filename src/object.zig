const std = @import("std");
const StringHashMap = std.StringHashMap;
const HashMap = @import("hash_map.zig").HashMap;
const ArrayList = std.ArrayList;
const ast = @import("ast.zig");
const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;
const print = std.debug.print;

const Allocator = std.mem.Allocator;

pub const Object = union(enum) {
    integer: i32,
    boolean: bool,
    nullable,
    return_val_obj: ReturnObject,
    function: FunctionObject,

    pub fn deinit(obj: *const Object) void {

        // print("Deinitalizing object\n", .{});
        switch (obj.*) {
            .return_val_obj => |rvj| {
                print("Deinitalizing return object\n", .{});
                rvj.deinit();
            },
            .function => |func| {
                print("Deinitalizing func object\n", .{});
                func.deinit();
            },

            else => {}
        }

    }
    
    pub fn clone(obj: *const Object) !Object {
        switch (obj.*) {
            .integer, .boolean, .nullable => {
                return obj.*;
            }, 
            inline else => |case| {
                return case.clone();
            }
        }
    }


    /// return string of value, str need to be deallocated by caller
    pub fn inspect(obj: *const Object, allocator: Allocator) ![]const u8 {
        
        switch (obj.*) {
            .nullable => {
                const str = try std.fmt.allocPrint(allocator, "null", .{});
                return str;
            },
            inline else => |case| {
                const str = try std.fmt.allocPrint(allocator, "{}", .{case});
                return str;
            }

        }
    }

};

const ReturnObject = struct {
    allocator: Allocator,
    value: *const Object,

    pub fn deinit(ret_obj: *const ReturnObject) void {
        print("deinits ret obj\n", .{});
        ret_obj.value.deinit();
        ret_obj.allocator.destroy(ret_obj.value);
    }

    pub fn clone(ro: *const ReturnObject) Allocator.Error!Object {
        const value_ptr = try ro.allocator.create(Object);
        value_ptr.* = try ro.value.clone();

        return Object {
            .return_val_obj = .{
                .allocator = ro.allocator,
                .value = value_ptr,
            }
        };

    }

};

pub const FunctionObject = struct {
    params: ?ArrayList(Identifier),
    body: BlockStatement,
    env: *Environment,

    fn deinit(fnc_obj: *const FunctionObject) void {

        print("deinits fn obj, addr: {*}\n", .{fnc_obj});

        fnc_obj.body.deinit();

        if (fnc_obj.params) |params| {

            for (params.items) |p| {
                p.deinit();
            }
            params.deinit();
        }
        // fnc_obj.env.deinit();
        //
    }

    pub fn clone(fo: *const FunctionObject) Allocator.Error!Object {

        print("cloned func\n", .{});

        var params: ?ArrayList(Identifier) = null;

        if (fo.params) |old_params| {

            params = ArrayList(Identifier).init(old_params.allocator);

            for (old_params.items) |p| {

                try params.?.append(try p.clone());

            }
        }

        const body = try fo.body.clone();

        return Object {
            .function = .{
                .params = params,
                .body = body,
                .env = fo.env,
            }
        };

    }

};

pub const Environment = struct {
    store: HashMap(Object),
    outer: ?*Environment = null,

    pub fn init(allocator: Allocator) Environment {
        return Environment {
            .store = HashMap(Object).init(allocator)
        };
    }

    pub fn initClosedEnv(env: *Environment) Environment {
        return Environment {
            .store =  HashMap(Object).init(env.store.allocator),
            .outer = env
        };
    }

    /// Returns cloned object in env or null.
    /// First checks its own store, if that returns null, 
    /// it checks outer.
    pub fn get(env: *Environment, key: []const u8) !?Object {
        //cloooone obj

        var maybe_obj = env.store.get(key);

        if (maybe_obj) |obj|  {
            
            print("return clone of env obj\n", .{});
            return try obj.clone();

        } else { // if maybe_obj == null
            if (env.outer) |outer| { // if env.out != null
                maybe_obj = try outer.get(key);

                if (maybe_obj) |obj| {
                    print("return clone of outer env obj\n", .{});
                    return try obj.clone();
                }
            } 
            
            return null;
        }


    }

    pub fn deinit(env: *Environment) void {
        print("deinits env\n", .{});
        env.store.deinit();
    }

};



