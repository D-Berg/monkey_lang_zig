const std = @import("std");

const ast = @import("../ast.zig");
const Environment = @import("../Environment.zig");

const log = std.log.scoped(.FunctionObject);
const Allocator = std.mem.Allocator;

const Identifier = ast.Identifier;
const BlockStatement = ast.BlockStatement;

const FunctionObject = @This();

params: []const Identifier,
body: BlockStatement,
env: *Environment,
rc: usize = 0,

pub fn deinit(func_obj: *const FunctionObject, allocator: Allocator) void {

    // only deinit if obj dont have a owner
    log.debug("trying to deinit fn object {*}", .{func_obj});
    if (func_obj.rc == 0) {
        defer log.debug("finished deinited fn object {*}", .{func_obj});

        // Only deinit if fnc_obj dont have a owner

        log.debug("func_obj ref count is {} , starting deinit\n", .{func_obj.rc});

        log.debug("deiniting fn body", .{});
        func_obj.body.deinit(allocator);

        log.debug("allocator: freeing fn params", .{});
        for (func_obj.params) |p| allocator.free(p.token.literal);
        allocator.free(func_obj.params);

        log.debug("trying to deinit env, {*}", .{func_obj.env});

        var current_env = func_obj.env;
        var n_destroyed: usize = 0;
        while (current_env.outer) |outer| : (n_destroyed += 1) {
            log.debug("had outer", .{});
            log.debug("deinits func objects enclosed env: {*}({})\n", .{ current_env, current_env.kind });
            if (current_env.rc > 0) current_env.rc -= 1;
            current_env.deinit(allocator);
            log.debug("allocator: destroying {*}({})", .{ current_env, current_env.kind });
            if (current_env.rc == 0) allocator.destroy(current_env);
            current_env = outer;
            log.debug("updated current_env to {*}({})", .{ current_env, current_env.kind });
        }
        log.debug("destroyed {} environments", .{n_destroyed});
    } else {
        log.debug("didnt deinits fnc_obj {*} since its referenced by {} other objects\n", .{ func_obj, func_obj.rc });
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

pub fn format(
    self: FunctionObject,
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("fn (", .{});
    for (self.params, 0..) |param, i| {
        try writer.print("{s}", .{param.tokenLiteral()});
        if (i + 1 < self.params.len) try writer.print(", ", .{});
    }
    try writer.print(")", .{});
    try writer.print("{{ {f} }}", .{self.body});
}
