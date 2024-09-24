const std = @import("std");
const Allocator = std.mem.Allocator;
const object = @import("object.zig");
const Object = object.Object;
const Token = @import("Token.zig");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const expect = std.testing.expect;


const m: usize = std.math.pow(usize, 10, 9) + 9;

/// A HashMap for Objects using strings as key
/// Clones value and key
pub fn HashMap(comptime V: type) type { // TODO: Remove generic

    const p = 31;

    return struct {
        const Self = @This();
        const Entry = struct {
            key: []const u8,
            val: V,
            // next: ?*Entry = null,
            // previous: ?*Entry,
        };

        allocator: Allocator,
        n_entries: usize = 0,
        buckets: ?[]?Entry = null,

        pub fn init(allocator: Allocator) Self {

            return Self {
                .allocator = allocator
            };
        }
        
        pub fn deinit(self: *Self) void {

            if (self.buckets) |buckets| {

                for (buckets) |bucket| {

                    if (bucket) |entry| {
                        self.allocator.free(entry.key);
                        entry.val.deinit();
                    }

                }
                    
                self.allocator.free(buckets);

            }

            
            

        }

        /// Clones the object
        pub fn put(self: *Self, key: []const u8, value: V) Allocator.Error!void {
            
            // print("got key: {s}, value: {}\n", .{key, value});
            
            if (self.buckets == null) {
                self.buckets = try self.allocator.alloc(?Entry, 2);

                for (self.buckets.?) |*bucket| {
                    bucket.* = null;
                }
            }

            // TODO if collisions increase buckets until no collision
            // TODO:

            
            if (self.checkCollision(key)) {
                print("collision!!!!\n", .{});
                try self.resize();
            }
            // print("no collision\n", .{});

            const h = hash(key);

            const b_idx = h % self.buckets.?.len;
            // print("putting {s} entry in {}\n", .{key, b_idx});
            
            const key_str = try self.allocator.alloc(u8, key.len);
            std.mem.copyForwards(u8, key_str, key);

            self.buckets.?[b_idx] = Entry {
                .key = key_str,
                .val = try value.clone()
            };

            self.n_entries += 1;
        }


        pub fn get(self: *Self, key: []const u8) ?V {

            const h = hash(key);
            
            if (self.buckets) |buckets| {
                
                const maybe_entry = buckets[h % buckets.len];
                
                if (maybe_entry) |entry| {
                    
                    if (std.mem.eql(u8, entry.key, key)) {
                        return entry.val;
                    } 

                    return null;
                
                } else {
                    return null;
                }
                

            } else {
                return null;
            }

        }
        // TODO: delete
    

        // https://cp-algorithms.com/string/string-hashing.html
        fn hash(key: []const u8) usize {
            var h: usize = 0;
            var p_pow: usize = 1;

            for (key) |c| {
                h +=  (c * p_pow) % m;
                p_pow *= p % m;
            }
            
            return h;
        }

        fn checkCollision(self: *Self, key: []const u8) bool{

            // print("CHECKING COLL\n", .{});

            const b_idx = hash(key) % self.buckets.?.len;

            const maybe_entry = self.buckets.?[b_idx];

            if (maybe_entry) |entry| { 
                
                // print("entry is not empty, new key = {s}, old key = {s}\n", .{key, entry.key});

                
                if (std.mem.eql(u8, entry.key, key)) { // no collision
                    // we want to update value
                    return false;
                }

                return true;

            } else {
                // bucket is empty 
                return false; // no collision
            }


        }

        
        fn resize(self: *Self) Allocator.Error!void {
            const new_size = self.buckets.?.len * 2;
            
            // print("\nresizing to {} buckets\n", .{new_size});
            
            const old_buckets = self.buckets.?;
            self.buckets = try self.allocator.alloc(?Entry, new_size);
            
            for (self.buckets.?) |*bucket| {
                bucket.* = null;
            }

            for (old_buckets) |maybe_entry| {
                    
                if (maybe_entry) |entry| {

                    // TODO: what the fuck happens if we collide when resizing??
                    try self.put(entry.key, entry.val);
                    self.allocator.free(entry.key);

                }

            }
            
            self.allocator.free(old_buckets);
            

        }

    };



}



test "init hashmap" {
    const allocator = std.testing.allocator;

    var token1 = try Token.init(allocator, .Ident, "a");
    defer token1.deinit();
    const obj1 = Object {.integer = 3};

    var token2 = try Token.init(allocator, .Ident, "b");
    defer token2.deinit();
    const obj2 = Object {.integer = 4};

    var token3 = try Token.init(allocator,.Ident, "c");
    defer token3.deinit();
    const obj3 = Object {.integer = 5};

    var store = HashMap(Object).init(allocator);
    defer store.deinit();
        
    try store.put(token1.tokenLiteral(), obj1);
    try store.put(token2.tokenLiteral(), obj2);
    try store.put(token3.tokenLiteral(), obj3);

    try expect(store.get(token1.tokenLiteral()).?.integer == obj1.integer);
    
    
    // try store.put(obj);

    // print("created env store: {any}\n", .{store.buckets.?});

}
