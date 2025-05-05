const std = @import("std");
const Allocator = std.mem.Allocator;
const debug_print = std.debug.print;
const log = std.log.scoped(.@"Environment");

const Object = @import("object.zig").Object;
const HashMap = @import("hash_map.zig").HashMap;
const StringHashMap = std.StringHashMapUnmanaged;

const Environment = @This();

store: StringHashMap(Object),
outer: ?*Environment,
rc: usize,
kind: Kind,

const Kind = enum {
    main,
    enclosed
};
pub const empty = Environment {
    .store = .empty,
    .outer = null,
    .rc = 0,
    .kind = .main,
};


/// Creates a new Environment wich references the outer and 
/// increases the rc of outer.
/// Starts with rc of 1 since its going to be used by a function.
pub fn enclosed(outer: *Environment) Environment {
    log.debug("initializing enclosed env, setting outer to {*}\n", .{outer});
    return Environment {
        .store = .empty,
        .outer = outer,
        .rc = 1, // refereced by a function
        .kind = .enclosed
    };
}


/// Decreases rc of outer and frees store
pub fn deinit(self: *Environment, allocator: Allocator) void {

    log.debug("trying to deinit env {*} of kind {s}\n", .{self, @tagName(self.kind)});

    if (self.rc == 0 or self.isMainEnv()) {
        defer log.debug("deinited env {*} of kind {s}\n", .{self, @tagName(self.kind)});

        // log.debug(, args: anytype)
        var hm_it = self.store.iterator();
        while (hm_it.next()) |e|  {

            log.debug("allocator: freeing key({*}){s}", .{e.key_ptr, e.key_ptr.*});
            allocator.free(e.key_ptr.*);

            const val = e.value_ptr;
            log.debug("deiniting value({s})", .{@tagName(val.*)});
            switch (val.*) {
                .function => |f| f.rc -= 1,
                .string => |s| s.rc -= 1,
                .array => |a| a.rc -= 1,
                else => {}
            }
            val.deinit(allocator);
        }

        self.store.deinit(allocator);

        // if (self.outer) |outer| {
        //     if (outer.rc > 0)outer.rc -= 1;
        //     log.debug("destryoing env ptr {*} of kind {}", .{self, self.kind});
        //     allocator.destroy(self);
        //     // self.outer = null;
        //     if (!outer.isMainEnv()) outer.deinit(allocator); // try to deinit outer
        // }
    } else {
        log.debug("didnt deinit env {*} since its referenced by {} others\n", .{ self, self.rc });
    }
}

fn isMainEnv(env: *Environment) bool {
    if (env.outer == null) {
        return true;
    } else {
        return false;
    }
}

/// debuging
pub fn print(env: *Environment) void {
    log.debug("env contians:\n", .{});
    var it = env.store.iterator();

    while  (it.next()) |entry| {
        log.debug("key: {s}, val: {}\n", .{entry.key_ptr.*, entry.value_ptr.*});
    }

    if (env.outer) |outer| {
        log.debug("printing outer\n\n", .{});
        outer.print();
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

/// Increases some objects ref count and store them in env (hash_map)
// TODO: fix deallocation for collisions
pub fn put(env: *Environment, allocator: Allocator, key: []const u8, val: Object) Allocator.Error!void {


    log.debug("putting obj({s}) {s}  in env: {*}({s})", .{@tagName(val), key, env, @tagName(env.kind)});


    if (env.store.getPtr(key)) |current_val| {
        current_val.destroy(allocator); //replace current_val
        current_val.* = val;
    } else {
        const key_copy = try allocator.dupe(u8, key);
        errdefer allocator.free(key_copy);
        // increase rc of refcounted objects
        switch (val) {
            .function => |f| f.rc += 1,
            .string => |s| s.rc += 1,
            .array => |a| a.rc += 1,
            else => {}
        }

        try env.store.put(allocator, key_copy, val);
    }
}

/// First checks its own store, if that returns null,
/// it checks outer.
pub fn get(env: *Environment, key: []const u8) ?Object {

    // p.146
    log.debug("Retreiving {s} from env: {*}\n", .{key, env});
    if (env.store.get(key)) |val| {
        log.debug("found {s}({s}) in env {*}", .{key, @tagName(val), env});
        return val;
    }

    if (env.outer) |outer| {
        return outer.get(key);
    }

    return null;

}
