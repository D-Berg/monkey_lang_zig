const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const log = std.log.scoped(.runtime);

pub const std_options: std.Options = .{
    .log_level = .debug,
};

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
var global_allocator = debug_allocator.allocator();

var stdout_buffer: [1024]u8 = undefined;
var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
const stdout = &stdout_writer.interface;

const GC = struct {
    roots: ArrayList(*GC.Node) = .empty,
    objects: ArrayList(*GC.Node) = .empty,

    fn deinit(self: *GC, gpa: Allocator) void {
        for (0..self.objects.items.len) |i| {
            gc.destroyObject(gpa, i);
        }

        self.roots.deinit(gpa);
        self.objects.deinit(gpa);
    }

    const Node = struct {
        kind: Kind,
        /// if its marked to be garbage collected
        marked: bool = false,
        children: ArrayList(*GC.Node) = .empty,

        const Kind = enum(i32) {
            string,
        };
    };

    fn collect(self: *GC) void {
        // mark
        for (self.roots.items) |root| {
            mark(root);
        }

        // sweep
        for (self.objects.items, 0..) |node, i| {
            if (node.marked) {
                node.marked = false;
            } else {
                self.destroyObject(i);
            }
        }
    }

    fn destroyObject(
        self: *GC,
        gpa: Allocator,
        node_idx: usize,
    ) void {
        const node = self.objects.swapRemove(node_idx);

        switch (node.kind) {
            .string => {
                const obj: *ObjectString = @fieldParentPtr("node", node);
                obj.deinit(global_allocator);
                gpa.destroy(obj);
            },
        }
    }

    fn mark(node: *GC.Node) void {
        node.marked = true;
        for (node.children) |child| {
            mark(child);
        }
    }
};

const ObjectString = struct {
    str: []u8,
    node: GC.Node = .{ .kind = .string },

    fn deinit(self: *ObjectString, gpa: Allocator) void {
        self.node.children.deinit(gpa);
        gpa.free(self.str);
    }
};

var gc = GC{};

extern fn __monkey_main() i32;

export fn __allocate_string(addr: [*]const u8, len: usize) isize {
    log.debug("allocating string obj '{s}' of len {}", .{ addr[0..len], len });

    const slice = global_allocator.alloc(u8, len) catch return -1;

    const obj_str = global_allocator.create(ObjectString) catch |err| {
        log.err("Failed to create str object: {}", .{err});
        global_allocator.free(slice);
        return -1;
    };

    obj_str.* = ObjectString{
        .str = slice,
    };

    gc.roots.append(global_allocator, &obj_str.node) catch |err| {
        log.err("Failed to append root: {}", .{err});
        obj_str.deinit(global_allocator);
        return -1;
    };

    @memcpy(obj_str.str, addr[0..len]);

    return @intCast(@intFromPtr(&obj_str.node));
}

export fn __print_object(gc_node_addr: usize) void {
    log.debug("printing obj...", .{});
    const gc_node: *GC.Node = @ptrFromInt(gc_node_addr);

    switch (gc_node.kind) {
        .string => {
            const obj_str: *ObjectString = @fieldParentPtr("node", gc_node);
            log.debug("str = {s}", .{obj_str.str});

            stdout.print("{s}", .{obj_str.str}) catch |err| {
                log.debug("Failed to print string, {}", .{err});
            };
            stdout.flush() catch return;
        },
        // else => {},
    }
}

pub fn main() !void {
    const rc = __monkey_main();

    gc.deinit(global_allocator);

    std.debug.print("got {} back from monkey_main\n", .{rc});
    const check = debug_allocator.deinit();
    std.debug.print("allocation: {}\n", .{check});
}
