const std = @import("std");
const StringHashMap = std.StringHashMap;
const HashMap = @import("hash_map.zig").HashMap;
const ArrayList = std.ArrayList;
const ast = @import("ast.zig");
const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;

const Allocator = std.mem.Allocator;

pub const Object = union(enum) {
    integer: i32,
    boolean: bool,
    nullable,
    return_val_obj: ReturnObject,
    function: FunctionObject,

    pub fn deinit(obj: *const Object) void {

        if (obj.* == .return_val_obj) {
            
            obj.return_val_obj.deinit();

        }

        if (obj.* == .function) {
            
            obj.function.deinit();

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

    pub fn get(env: *Environment, key: []const u8) ?Object {

        var maybe_obj = env.get(key);

        if (maybe_obj == null and env.outer != null)  {
            maybe_obj = env.outer.?.get(key);
        }

        return maybe_obj;

    }

    pub fn deinit(env: *Environment) void {
        env.store.deinit();
    }

};



