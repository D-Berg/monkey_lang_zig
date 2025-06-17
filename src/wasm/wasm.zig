const std = @import("std");
pub const wasm = @This();
pub const Module = @import("Module.zig");
pub const Section = @import("Section.zig");
pub const TypeSection = @import("TypeSection.zig");
pub const FunctionSection = @import("FunctionSection.zig");
pub const ExportSection = @import("ExportSection.zig");
pub const CodeSection = @import("CodeSection.zig");

pub const MAGIC_MODULE_HEADER = [4]u8{ 0x00, 0x61, 0x73, 0x6d };
pub const MODULE_VERSION = [4]u8{ 0x01, 0x00, 0x00, 0x00 };

// https://pengowray.github.io/wasm-ops/
pub const OpCode = enum(u8) {
    end = 0x0b,
    @"return" = 0x0f,
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
    @"i64.add" = 0x7c,

    _,
};

pub const Type = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    funcref = 0x70,
    externref = 0x6f,
    function = 0x60,
    result = 0x40,

    pub fn byte(self: Type) u8 {
        return @intFromEnum(self);
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
    param_types: []const wasm.Type,
    return_types: []const wasm.Type,
    body: []const wasm.OpCode,
    locals: std.enums.EnumMap(wasm.Type, u32) = .init(.{}),
    @"export": bool = false,
    name: []const u8 = "",
};

// TODO: make it decode as well
pub fn ULEB128Encoder(T: type) type {
    const len = @sizeOf(T) / @sizeOf(u8);
    return struct {
        const Self = @This();

        pub const init: Self = .{};

        buf: [len]u8 = undefined,

        /// retruns the length of encoding
        pub fn encode(self: *Self, value: T) []const u8 {
            var val = value;
            var i: usize = 0;

            while (true) {
                var byte: u8 = @intCast(val & 0x7F);
                val >>= 7;

                if (val != 0) {
                    byte |= 0x80; // More bytes follow
                }

                self.buf[i] = byte;
                i += 1;

                if (val == 0) break;
            }

            return self.buf[0..i];
        }
    };
}
