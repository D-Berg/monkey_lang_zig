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
    function: *FunctionObject,

    pub fn deinit(obj: *const Object) void {

        // print("Deinitalizing object\n", .{});
        switch (obj.*) {
            .return_val_obj => |rvj| {
                // print("Deinitalizing return object\n", .{});
                rvj.deinit();
            },
            .function => |func| {
                // print("Deinitalizing func object\n", .{});
                _ = func;
                // func.deinit();
            },

            else => {}
        }

    }
    
    pub fn clone(obj: *const Object) !Object {
        switch (obj.*) {
            .integer, .boolean, .nullable => {
                return obj.*;
            }, 
            .function => {
                @panic("clone for FunctionObject not yet implemented");
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
    owner: ?*Environment,

    pub fn deinit(ret_obj: *const ReturnObject) void {
        // print("deinits ret obj\n", .{});
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
                .owner = ro.owner,
            }
        };

    }

};

pub const FunctionObject = struct {
    allocator: Allocator,
    params: ArrayList(Identifier),
    body: BlockStatement,
    env: *Environment,
    owner: ?*Environment, // the env that owns the object have the responsebility to destroy it, if null it should deallocate

    // fn deinit(fnc_obj: *const FunctionObject) void {
    //
    //     // TODO: only deinit if obj dont have a owner
    //
    //     print("deinits fn obj, addr: {*}\n", .{fnc_obj});
    //
    //     fnc_obj.body.deinit();
    //
    //     for (fnc_obj.params.items) |p| {
    //         p.deinit();
    //     }
    //     fnc_obj.params.deinit();
    //
    //     // print("env.outer = {?}\n", .{fnc_obj.env.outer});
    //
    //     // TODO deinit env if its not the outermost env
    //     if (fnc_obj.env.outer != null) {
    //         print("deinits func objects env: {*}\n", .{fnc_obj.env});
    //         fnc_obj.env.deinit();
    //         fnc_obj.allocator.destroy(fnc_obj.env);
    //     }
    //
    //     //
    // }

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
    pub fn String(fo: *const FunctionObject) Allocator.Error![]const u8 {

        const n_params = fo.params.items.len;
        
        if (fo.env.outer) |outer| {

            return try std.fmt.allocPrint(fo.allocator, "fn object: p: {}, env: {*}, outer_env: {*}", .{
                n_params, fo.env, outer
            });
        }
        return try std.fmt.allocPrint(fo.allocator, "fn object: p: {}, env: {*}, outer_env: {?}", .{
            n_params, fo.env, fo.env.outer
        });

    }

};

pub const Environment = struct {
    store: HashMap(),
    outer: ?*Environment = null,

    pub fn init(allocator: Allocator) Environment {
        return Environment {
            .store = HashMap().init(allocator)
        };
    }

    pub fn deinit(env: *Environment) void {
        print("deinits env {*}\n", .{env});
        env.store.deinit();
    }

    pub fn printEnv(env: *Environment) void {

        print("printing env {*}\n", .{env});
        env.store.printHM();

        if (env.outer) |outer| {
            print("printing outer\n\n", .{});
            outer.printEnv();
        
        }

    }

    // pub fn clone(env: *Environment) Allocator.Error!*Environment {
    //
    //     print("cloning env: {*}\n", .{env});
    //     var new_env = Environment {
    //         .store = try env.store.clone(),
    //         .outer = env.outer,
    //     };
    //     errdefer new_env.deinit();
    //
    //     const new_env_ptr = try env.store.allocator.create(Environment);
    //     new_env_ptr.* = new_env;
    //
    //     return new_env_ptr;
    //
    // }

    pub fn initClosedEnv(outer: *Environment) Environment {
        return Environment {
            .store =  HashMap().init(outer.store.allocator),
            .outer = outer
        };
    }

    pub fn put(env: *Environment, key: []const u8, val: *Object) Allocator.Error!void {

        
        switch (val.*) {
            .function => {
                val.function.owner = env;
            },

            // TODO fill out more
            else => {}
        }

        try env.store.put(key, val.*);
        // TODO: set ownership of obj to env

    }

    /// Returns cloned object in env or null.
    /// First checks its own store, if that returns null, 
    /// it checks outer.
    pub fn get(env: *Environment, key: []const u8) ?Object {

        // p.146 
        print("Retreiving {s} from env: {*}\n", .{key, env});
        var maybe_obj_ptr = env.store.get(key);

        if (maybe_obj_ptr) |obj_ptr|  {
            
            // print("return clone of env obj\n", .{});
            print("found {s} in env {*}\n", .{key, env});
            return obj_ptr;

        } else { // if maybe_obj == null
            if (env.outer) |outer| { // if env.out != null
            
                print("didnt find {s} in enclosed, checking outer env\n", .{key});
                maybe_obj_ptr = outer.get(key);

                if (maybe_obj_ptr) |obj_ptr| {
                    // print("return clone of outer env obj\n", .{});
                    return obj_ptr;
                }
            } 
            
            return null;
        }


    }


};



