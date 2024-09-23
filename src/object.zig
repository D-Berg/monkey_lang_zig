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

        print("Deinitalizing object\n", .{});
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
            .function => |fo| {

                var params: ?ArrayList(Identifier) = null;

                if (fo.params != null) {
                    params = try fo.params.?.clone();
                }

                const body = try fo.body.clone();

                return Object {
                    .function = .{
                        .params = params,
                        .body = body,
                        .env = fo.env,
                    }
                };

            },
            else => {
                return obj.*;
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
        ret_obj.allocator.destroy(ret_obj.value);
    }

};

const FunctionObject = struct {
    params: ?ArrayList(Identifier),
    body: BlockStatement,
    env: *Environment,

    fn deinit(fnc_obj: *const FunctionObject) void {

        fnc_obj.body.statements.deinit(); // working

        if (fnc_obj.params) |params| {
            params.deinit();
        }
        // fnc_obj.env.deinit();
        //
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

    pub fn get(env: *Environment, key: []const u8) !?Object {

        //cloooone obj

        var maybe_obj = env.store.get(key);

        if (maybe_obj) |obj|  {
            
            return try obj.clone();

        } else { // if maybe_obj == null
            if (env.outer) |outer| { // if env.out != null
                maybe_obj = try outer.get(key);

                if (maybe_obj) |obj| {
                    return try obj.clone();
                }
            } 
            
            return null;
        }


    }

    pub fn deinit(env: *Environment) void {
        env.store.deinit();
    }

};



