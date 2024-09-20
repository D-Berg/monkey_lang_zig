const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Object = union(enum) {
    integer: IntegerObject,
    boolean: BoolObject,
    nullable: NullObject,

    /// return string of value, str need to be deallocated by caller
    pub fn inspect(obj: *Object, allocator: Allocator) []const u8 {
        
        switch (obj.*) {
            .nullable => {

                const str = try std.fmt.allocPrint(allocator, "null", .{});
                return str;
            },
            inline else => |*case| {
                const str = try std.fmt.allocPrint(allocator, "{}", .{case.value});
                return str;
            }

        }
    }

};

pub const IntegerObject = struct {
    value: i32,
};

pub const BoolObject = struct {
    value: bool,
};

pub const NullObject = struct {

};
