const std = @import("std");
const Environment = @import("Environment.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("evaluator.zig");
var write_buffer: [1000]u8 = undefined;
const bufPrint = std.fmt.bufPrint;

const allocator = std.heap.wasm_allocator;
const log = std.log.scoped(.Wasm);

pub const std_options = std.Options{
    // Set the log level to info
    .log_level = .debug,

    // Define logFn to override the std implementation
    .logFn = wasmLogFn,
};

extern fn write(fd: i32, buf: usize, count: usize) i32;

fn println(buf: []const u8) void {
    _ = write(1, @intFromPtr(buf.ptr), buf.len);
}

export fn alloc(len: usize) usize {
    const mem = allocator.alloc(u8, len) catch {
        return 0;
    };
    return @intFromPtr(mem.ptr);
}

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn wasmLogFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const scope_prefix = "(" ++ @tagName(scope) ++ ")";

    const prefix = "[" ++ comptime level.asText() ++ "] " ++ scope_prefix;

    // Print the message to stderr, silently ignoring any errors
    //
    const log_str = bufPrint(&write_buffer, prefix ++ format ++ "\n", args) catch return;
    println(log_str);
}

pub export fn web_main(c_input: [*:0]const u8) u32 {
    const sentinal_idx = std.mem.indexOfSentinel(u8, 0, c_input);

    run(c_input[0..sentinal_idx]) catch |err| {
        log.err("failed to execute with error {}", .{err});
        return 420;
    };

    log.info("Program Succeded", .{});
    return 69;
}

fn run(input: []const u8) !void {
    log.debug("input = {s}", .{input});

    var env: Environment = .empty;
    defer env.deinit(allocator);

    var parser = Parser.init(allocator, input);

    var program = try parser.Program(allocator);
    defer program.deinit(allocator);

    const maybe_evaluated = try evaluator.eval(allocator, &program, &env);

    if (maybe_evaluated) |evaluated| {
        defer evaluated.deinit(allocator);
        const eval_str = try evaluated.inspect(allocator);
        defer allocator.free(eval_str);
        log.info("evaluated: {s}", .{eval_str});
    }
}
