const std = @import("std");
const bufPrint = std.fmt.bufPrint;
const allocator = std.heap.wasm_allocator;

const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Parser = @import("Parser.zig");

const log = std.log.scoped(.Wasm);

const STDOUT_FILENO = 1;
const STDERR_FILENO = 2;

// Posix "file"
const FakeFile = struct {
    handle: i32,

    fn writer(self: *const FakeFile, buffer: []u8) Writer {
        return Writer{ .file = self, .interface = std.Io.Writer{
            .buffer = buffer,
            .vtable = &.{ .drain = Writer.drain },
        } };
    }

    const Writer = struct {
        file: *const FakeFile,
        interface: std.Io.Writer,

        fn drain(io_w: *std.Io.Writer, data: []const []const u8, splat: usize) !usize {
            const w: *Writer = @fieldParentPtr("interface", io_w);

            const buffered = io_w.buffered();

            if (buffered.len != 0) {
                const n = write(w.file.handle, buffered) catch {
                    return error.WriteFailed;
                };

                return io_w.consume(n);
            }

            if (data.len == 0) return 0;

            for (data[0 .. data.len - 1]) |buf| {
                const n = write(w.file.handle, buf) catch {
                    return error.WriteFailed;
                };

                return io_w.consume(n);
            }

            const pattern = data[data.len - 1];
            if (pattern.len == 0 or splat == 0) return 0;

            var n: usize = 0;
            for (0..splat) |_| {
                n += write(w.file.handle, pattern) catch {
                    return error.WriteFailed;
                };
            }
            return io_w.consume(n);
        }
    };
};

const stdout_file = FakeFile{
    .handle = STDOUT_FILENO,
};
var stdout_buffer: [256]u8 = undefined;
var stdout_writer = stdout_file.writer(&stdout_buffer);
const stdout = &stdout_writer.interface;

const stderr_file = FakeFile{
    .handle = STDERR_FILENO,
};
var stderr_buffer: [256]u8 = undefined;
var stderr_writer = stderr_file.writer(&stderr_buffer);
const stderr = &stderr_writer.interface;

pub const std_options = std.Options{
    // Set the log level to info
    .log_level = .err,

    // Define logFn to override the std implementation
    .logFn = wasmLogFn,
};

extern fn js_write(fd: i32, buf: usize, count: usize) i32;

fn write(fd: i32, bytes: []const u8) !usize {
    const rc = js_write(fd, @intFromPtr(bytes.ptr), bytes.len);
    if (rc < 0) return error.FailedToWrite;
    return @intCast(rc);
}

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

    stderr.print(level_txt ++ prefix2 ++ format ++ "\n", args) catch return;
    stderr.flush() catch return;
}

export fn wasm_evaluate(input_ptr_int: usize, len: usize) usize {
    var input: []u8 = undefined;
    input.ptr = @ptrFromInt(input_ptr_int);
    input.len = len;

    eval(input, stdout) catch |err| {
        log.err("Failed to evalutate, error: {s}", .{@errorName(err)});
        return @intFromError(err);
    };

    return 0;
}

fn eval(input: []const u8, writer: *std.Io.Writer) !void {
    log.debug("got input = {s}", .{input});

    var env: Environment = .empty;
    defer env.deinit(allocator);

    var parser = Parser.init(input);

    var program = try parser.Program(allocator);
    defer program.deinit(allocator);

    const evaluated = try evaluator.eval(allocator, &program, &env);
    defer evaluated.deinit(allocator);

    try writer.print("{f}\n", .{evaluated});
    try writer.flush();
}
