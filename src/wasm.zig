const std = @import("std");
const ArrayList = std.ArrayListUnmanaged;
const Allocator = std.mem.Allocator;
pub const wasm = @This();
pub const Module = @import("wasm/Module.zig");
pub const Section = @import("wasm/Section.zig");
pub const TypeSection = @import("wasm/TypeSection.zig");
pub const FunctionSection = @import("wasm/FunctionSection.zig");
pub const ExportSection = @import("wasm/ExportSection.zig");
pub const CodeSection = @import("wasm/CodeSection.zig");
pub const GlobalSection = @import("wasm/GlobalSection.zig");

pub const MAGIC_MODULE_HEADER = [4]u8{ 0x00, 0x61, 0x73, 0x6d };
pub const MODULE_VERSION = [4]u8{ 0x01, 0x00, 0x00, 0x00 };

// https://pengowray.github.io/wasm-ops/
pub const OpCode = enum(u8) {
    @"if" = 0x04,
    @"else" = 0x05,
    end = 0x0b,
    @"return" = 0x0f,
    /// throws away a single operand
    drop = 0x1a,
    @"local.get" = 0x20,
    /// # local.set x
    /// This instruction sets the value of a variable
    ///
    /// # Followed by
    /// `u32 x: localidx`
    @"local.set" = 0x21,
    /// # i32.const n
    /// push an i32-value to the stack
    ///
    /// # followed by
    /// n: `i32`
    @"i32.const" = 0x41,
    @"i64.const" = 0x42,

    @"i32.eqz" = 0x45,
    /// ==
    @"i32.eq" = 0x46,
    /// !=
    @"i32.neq" = 0x47,
    /// <
    @"i32.lt_s" = 0x48,
    /// >
    @"i32.gt_s" = 0x4a,

    @"i32.add" = 0x6a,
    @"i32.sub" = 0x6b,
    @"i32.mul" = 0x6c,
    @"i32.div_s" = 0x6d,

    @"i64.add" = 0x7c,
    @"i64.sub" = 0x7d,
    @"i64.mul" = 0x7e,
    @"i64.div_s" = 0x7f,

    @"f32.neg" = 0x8c,

    @"f32.convert_i32_s" = 0xb2,
    @"i32.reinterpret_f32" = 0xbc,
    _,
};

pub const ValType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    funcref = 0x70,
    externref = 0x6f,
    function = 0x60,
    result = 0x40,

    pub fn byte(self: ValType) u8 {
        return @intFromEnum(self);
    }

    pub fn opcode(self: ValType) OpCode {
        return @enumFromInt(self.byte());
    }
};

pub const ExportDescription = struct {
    kind: Kind,
    idx: u32,

    pub const Kind = enum(u8) {
        function = 0x00,
        table = 0x01,
        memory = 0x02,
        global = 0x03,
    };
};

pub const Function = struct {
    param_types: []const wasm.ValType,
    return_types: []const wasm.ValType,
    body: ArrayList(wasm.OpCode),
    locals: std.enums.EnumMap(wasm.ValType, u32),
    @"export": bool,
    name: []const u8,

    pub const init = Function{
        .param_types = &[_]wasm.ValType{},
        .return_types = &[_]wasm.ValType{},
        .body = .empty,
        .locals = .init(.{}),
        .@"export" = false,
        .name = "",
    };

    pub fn deinit(self: *Function, gpa: Allocator) void {
        self.body.deinit(gpa);
    }
};

// TODO: make it decode as well
pub fn LEB128Encoder(T: type) type {
    const len = @sizeOf(T) / @sizeOf(u8);

    if (!(T == u32 or
        T == u64 or
        T == i32 or
        T == i64)) @compileError("unsupported type");

    const isSigned: bool = if (T == u32 or T == u64)
        false
    else
        true;

    return struct {
        const Self = @This();

        pub const init: Self = .{};

        buf: [len]u8 = undefined,

        /// retruns the length of encoding
        pub fn encode(self: *Self, value: T) []const u8 {
            var val = value;
            var i: usize = 0;

            while (true) {
                if (isSigned) {
                    var byte: u8 = @intCast(val & 0x7F);
                    const sign_bit_set = (byte & 0x40) != 0;

                    val >>= 7;

                    const done = (val == 0 and !sign_bit_set) or (val == -1 and sign_bit_set);

                    if (!done) {
                        byte |= 0x80; // More bytes follow
                    }

                    self.buf[i] = byte;
                    i += 1;

                    if (done) break;
                } else {
                    var byte: u8 = @intCast(val & 0x7F);
                    val >>= 7;

                    if (val != 0) {
                        byte |= 0x80; // More bytes follow
                    }

                    self.buf[i] = byte;
                    i += 1;

                    if (val == 0) break;
                }
            }
            return self.buf[0..i];
        }

        /// Returns the decoded value and the number of bytes the encoded
        /// integer was.
        pub fn decode(self: *Self, bytes: []const u8) !struct { T, usize } {
            _ = self;
            var result: T = 0;
            var i: usize = 0;

            while (i < bytes.len and i < len) : (i += 1) {
                const byte = bytes[i];

                result += @as(T, @intCast(byte & 0x7f)) << @intCast(i * 7);

                if (byte & 0x80 == 0) return .{ result, i + 1 };
            }

            return error.Invalid128encoding;
        }
    };
}

test "LEB" {
    const expected_u64 = [_]u8{ 0xe5, 0x8e, 0x26 };
    var u64_encoder = LEB128Encoder(u64).init;

    try std.testing.expectEqualSlices(u8, &expected_u64, u64_encoder.encode(624485));

    const u64_decoded, const enc_len = try u64_encoder.decode(&expected_u64);
    try std.testing.expectEqual(3, enc_len);

    try std.testing.expectEqual(u64_decoded, 624485);
}
