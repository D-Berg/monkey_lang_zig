const std = @import("std");
const Allocator = std.mem.Allocator;

const trace = @import("tracy.zig").trace;

pub fn parse(gpa: Allocator, args: []const []const u8) CliArgs {
    const tracy = trace(@src());
    defer tracy.end();

    if (args.len == 0) return .repl;
    if (std.mem.eql(u8, args[0], "build")) return parseBuildArgs(gpa, args[1..]);
    if (std.mem.eql(u8, args[0], "run")) return parseRunArgs(gpa, args[1..]);

    return .{ .problem = "parsing arguments" };
}

fn parseBuildArgs(gpa: Allocator, args: []const []const u8) CliArgs {
    _ = gpa;
    const tracy = trace(@src());
    defer tracy.end();

    if (args.len == 0) return .{ .problem = "Missing input file" };

    var out_name: ?[]const u8 = null;
    const flag_args = args[1..];

    std.debug.print("flag_args.len = {}", .{flag_args.len});

    for (flag_args, 0..) |arg, i| {
        std.log.debug("arg = {s}, i = {}", .{ arg, i });

        if (std.mem.eql(u8, arg, "-o")) {
            if (i + 1 < flag_args.len) {
                out_name = flag_args[i + 1];
            } else {
                return .{ .problem = "Missing output name when supplied -o flag" };
            }
        }
    }

    return CliArgs{
        .build = BuildArgs{
            .path = args[0],
            .out_name = out_name orelse "main.wasm",
        },
    };
}

fn parseRunArgs(gpa: Allocator, args: []const []const u8) CliArgs {
    _ = gpa;
    const tracy = trace(@src());
    defer tracy.end();

    if (args.len == 0) return .{ .problem = "Missing input file" };

    return .{ .run = .{ .path = args[0] } };
}

pub const CliArgs = union(enum) {
    repl: void,
    run: RunArgs,
    build: BuildArgs,
    help: []const u8,
    problem: []const u8,
};

pub const RunArgs = struct {
    /// path to monkey file
    path: []const u8,
};

pub const BuildArgs = struct {
    path: []const u8,
    out_name: []const u8,
};
