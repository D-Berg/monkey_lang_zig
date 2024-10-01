const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Object = @import("object.zig").Object;
const HashMap = @import("hash_map.zig").HashMap;

const Environment = @This();

store: HashMap(),
outer: ?*Environment = null,

pub fn init(allocator: Allocator) Allocator.Error!Environment {
    return Environment {
        .store = try HashMap().init(allocator)
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

pub fn initClosedEnv(outer: *Environment) Allocator.Error!Environment {
    return Environment {
        .store = try HashMap().init(outer.store.allocator),
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
    // print("Retreiving {s} from env: {*}\n", .{key, env});
    var maybe_obj_ptr = env.store.get(key);

    if (maybe_obj_ptr) |obj_ptr|  {
        
        // print("return clone of env obj\n", .{});
        // print("found {s} in env {*}\n", .{key, env});
        return obj_ptr;

    } else { // if maybe_obj == null
        if (env.outer) |outer| { // if env.out != null
        
            // print("didnt find {s} in enclosed, checking outer env\n", .{key});
            maybe_obj_ptr = outer.get(key);

            if (maybe_obj_ptr) |obj_ptr| {
                // print("return clone of outer env obj\n", .{});
                return obj_ptr;
            }
        } 
        
        return null;
    }


}
