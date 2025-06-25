const std = @import("std");
const wasm = @import("wasm.zig");
const Section = wasm.Section;
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.TypeSection);

const ArrayList = std.ArrayListUnmanaged;

const Self = @This();

functions: ArrayList(Function),

pub const Function = struct {
    param_types: []const wasm.ValType,
    return_types: []const wasm.ValType,

    /// Copies the data
    pub fn init(
        gpa: Allocator,
        param_types: []const wasm.ValType,
        return_types: []const wasm.ValType,
    ) !Self.Function {
        const param_copy = try gpa.dupe(wasm.ValType, param_types);
        errdefer gpa.free(param_copy);

        const return_copy = try gpa.dupe(wasm.ValType, return_types);

        return Function{
            .param_types = param_copy,
            .return_types = return_copy,
        };
    }

    pub fn deinit(self: *Function, gpa: Allocator) void {
        gpa.free(self.param_types);
        gpa.free(self.return_types);
    }
};

pub const empty = Self{ .functions = .empty };

pub fn deinit(ctx: *anyopaque, gpa: Allocator) void {
    const self: *Self = @ptrCast(@alignCast(ctx));

    for (self.functions.items) |*func| func.deinit(gpa);

    self.functions.deinit(gpa);
}

pub fn content(ctx: *anyopaque, gpa: Allocator) ![]const u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    var out: ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    const writer = out.writer(gpa);

    var u32_encoder = wasm.LEB128Encoder(u32).init;

    const n_funcs: u32 = @intCast(self.functions.items.len);

    log.debug("n_funcs = {}", .{n_funcs});

    try writer.writeAll(u32_encoder.encode(n_funcs));

    for (self.functions.items) |func| {
        const p_len: u32 = @intCast(func.param_types.len);
        const r_len: u32 = @intCast(func.return_types.len);

        try writer.writeByte(wasm.ValType.function.byte());

        try writer.writeAll(u32_encoder.encode(p_len));
        try writer.writeAll(@ptrCast(func.param_types));

        try writer.writeAll(u32_encoder.encode(r_len));
        try writer.writeAll(@ptrCast(func.return_types));
    }

    return out.toOwnedSlice(gpa);
}

pub fn section(self: *Self) Section {
    return Section{
        .ptr = self,
        .vtable = &.{
            .deinit = deinit,
            .content = content,
            .parse = parse,
        },
        .id = .type,
    };
}

pub fn parse(ctx: *anyopaque, gpa: Allocator, bytes: []const u8) !void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    log.debug("decoding, len = {}, {x}", .{ bytes.len, bytes });

    var u32_converter = wasm.LEB128Encoder(u32).init;

    const n_funcs, var enc_len = try u32_converter.decode(bytes);

    log.debug("found {} functions", .{n_funcs});

    var at = enc_len;

    for (0..n_funcs) |i| {
        if (bytes[at] != wasm.ValType.function.byte()) {
            return error.InvalidValType;
        }
        at += 1;

        const n_params, enc_len = try u32_converter.decode(bytes[at..]);
        at += enc_len;

        const params = try gpa.alloc(wasm.ValType, n_params);
        defer gpa.free(params);

        log.debug("fn {} has {} params", .{ i, n_params });
        for (0..n_params) |p_idx| {
            params[p_idx] = @enumFromInt(bytes[at]);
            at += 1;
        }

        log.debug("params = {any}", .{params});

        const n_returns, enc_len = try u32_converter.decode(bytes[at..]);
        at += enc_len;

        const returns = try gpa.alloc(wasm.ValType, n_returns);
        defer gpa.free(returns);

        log.debug("fn {} has {} returns", .{ i, n_returns });

        for (0..n_returns) |r_idx| {
            returns[r_idx] = @enumFromInt(bytes[at]);
            at += 1;
        }

        var func: Function = try .init(gpa, params, returns);
        errdefer func.deinit(gpa);

        try self.functions.append(gpa, func);

        log.debug("returns = {any}", .{returns});
    }
}
