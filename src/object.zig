const std = @import("std");
const StringHashMap = std.StringHashMap;
const HashMap = @import("hash_map.zig").HashMap;

const Allocator = std.mem.Allocator;

pub const Object = union(enum) {
    integer: i32,
    boolean: bool,
    nullable,
    return_val: *const Object,

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

pub const Environment = struct {
    store: HashMap(Object),

    pub fn init(allocator: Allocator) Environment {
        return Environment {
            .store = HashMap(Object).init(allocator)
        };
    }

    pub fn deinit(env: *Environment) void {
        env.store.deinit();
    }

};



