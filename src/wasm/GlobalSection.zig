const std = @import("std");
const wasm = @import("wasm.zig");
const StringArrayHashMap = std.StringArrayHashMapUnmanaged;

globals: StringArrayHashMap(wasm.Module.Global.GlobalType),

const Self = @This();

pub const init = Self{ .globals = .empty };
