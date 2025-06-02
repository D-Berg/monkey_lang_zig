const std = @import("std");
const Environment = @import("Environment.zig");
const Parser = @import("Parser.zig");
const evaluator = @import("evaluator.zig");
var write_buffer: [1000]u8 = undefined;
const bufPrint = std.fmt.bufPrint;

const allocator = std.heap.wasm_allocator;
const log = std.log.scoped(.Wasm);

const STDIN_FILENO = 1;
const STDOUT_FILENO = 1;
const STDERR_FILENO = 2;

const GenericWriter = std.io.GenericWriter(File, anyerror, File.write);
// Posix "file"
const File = struct {
    handle: i32,

    fn write(self: File, bytes: []const u8) !usize {
        const written = js_write(self.handle, @intFromPtr(bytes.ptr), bytes.len);

        if (written < 0) return error.FailedToWrite;

        return @intCast(written);
    }

    fn writer(self: File) GenericWriter {
        return GenericWriter{ .context = self };
    }
};

const stdout = File{
    .handle = STDOUT_FILENO,
};

const stderr = File{
    .handle = STDERR_FILENO,
};

pub const std_options = std.Options{
    // Set the log level to info
    .log_level = .debug,

    // Define logFn to override the std implementation
    .logFn = wasmLogFn,
};

extern fn js_write(fd: i32, buf: usize, count: usize) i32;
extern fn read(fd: i32, buf: usize, count: usize) i32;

/// Allocated u8, to be called from js
export fn alloc(len: usize) usize {
    const mem = allocator.alloc(u8, len) catch {
        return 0;
    };
    const address: usize = @intFromPtr(mem.ptr);
    log.debug("allocated u8@{x}", .{address});
    return address;
}

/// free u8, to be called from js
export fn free(ptr_int: usize, len: usize) void {
    var slice: []u8 = undefined;
    const ptr: [*]u8 = @ptrFromInt(ptr_int);
    slice.ptr = ptr;
    slice.len = len;
    allocator.free(slice);
    log.debug("freed u8@{x}", .{ptr_int});
}

pub fn wasmLogFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const level_txt = comptime level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";

    // Print the message to stderr, silently ignoring any errors
    // const log_str = bufPrint(&write_buffer, prefix ++ format ++ "\n", args) catch return;

    const writer = stderr.writer();
    writer.print(level_txt ++ prefix2 ++ format ++ "\n", args) catch return;
}

pub export fn web_main(c_input: [*:0]const u8) u32 {
    const sentinal_idx = std.mem.indexOfSentinel(u8, 0, c_input);

    run(c_input[0..sentinal_idx]) catch |err| {
        log.err("failed to execute with error {s}", .{@errorName(err)});
        return 420;
    };

    log.info("Program Succeded", .{});
    return 69;
}

fn run(input: []const u8) !void {
    const writer = stdout.writer();
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
        try writer.print("evaluated {s}\n", .{eval_str});
    }
}
