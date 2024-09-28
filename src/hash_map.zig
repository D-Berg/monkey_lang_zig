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
pub fn HashMap() type { // TODO: Remove generic

    const p = 31;

    return struct {
        const Self = @This();
        const Entry = struct {
            allocator: Allocator,
            key: []const u8,
            val: Object,
            
            fn deinit(entry: *const Entry) void {
                entry.allocator.free(entry.key);
                entry.val.deinit();
            }
            
            // fn clone(entry: *const Entry) Allocator.Error!Entry {
            //
            //     const new_key = try entry.allocator.alloc(u8, entry.key.len);
            //     std.mem.copyForwards(u8, new_key, entry.key);
            //     errdefer entry.allocator.free(new_key);
            //
            //     return Entry {
            //         .allocator = entry.allocator,
            //         .key = new_key,
            //         .val = try entry.val.clone(),
            //     };
            //
            // }
        };

        allocator: Allocator,
        n_entries: usize = 0,
        buckets: []?ArrayList(Entry),

        pub fn init(allocator: Allocator) Allocator.Error!Self {
            
            var buckets = try allocator.alloc(?ArrayList(Entry), 2);
            
            for (0..buckets.len) |i| {
                buckets[i] = null;
            }

            return Self {
                .allocator = allocator,
                .buckets = buckets, // TODO change default size
            };
        }

        pub fn deinit(self: *Self) void {


            for (self.buckets) |maybe_bucket| {

                if (maybe_bucket) |bucket| {
                    
                    for (bucket.items) |entry| {
                        entry.deinit();
                    }
                    
                    bucket.deinit();
                    
                }

            }
                    
            self.allocator.free(self.buckets);

        }

        // pub fn clone(self: *Self) Allocator.Error!Self {
        //
        //     var new_buckets: ?[]?Entry = null;
        //
        //     if (self.buckets) |old_buckets| {
        //
        //         new_buckets = try self.allocator.alloc(?Entry, old_buckets.len);
        //
        //         for (old_buckets, 0..) |old_bckt, i| {
        //
        //             if (old_bckt) |old_entry| {
        //                 const new_entry = try old_entry.clone();
        //                 new_buckets.?[i] = new_entry;
        //             } else {
        //                 new_buckets.?[i] = null;
        //             }
        //
        //         }
        //     }
        //
        //     return Self {
        //         .allocator = self.allocator,
        //         .buckets = new_buckets,
        //         .n_entries = self.n_entries
        //     };
        //
        // }

        /// takes ownership of the objects
        pub fn put(self: *Self, key: []const u8, value: Object) Allocator.Error!void {
            
            print("got key: {s}, value: {}\n", .{key, value});
            
            // TODO if collisions increase buckets until no collision
            // TODO:
            const new_key_str = try self.allocator.alloc(u8, key.len);
            std.mem.copyForwards(u8, new_key_str, key);
        
            const new_entry = Entry {
                .allocator = self.allocator,
                .key = new_key_str,
                .val = value
            };
            errdefer new_entry.deinit();

            const h = hash(key);
            const b_idx = h % self.buckets.len;

            const maybe_bucket = self.buckets[b_idx];
            
            if (maybe_bucket) |bucket| {
                print("bucket not empty\n", .{});

                for (bucket.items, 0..) |entry, i| {
                    
                    if (std.mem.eql(u8, new_entry.key, entry.key)) {
                        print("key already exists, updading entry\n", .{});
                        entry.deinit(); 
                        bucket.items[i] = new_entry;
                        return;
                    }


                }

                print("appending to existing bucket\n", .{});
                
                try self.buckets[b_idx].?.append(new_entry);
                self.n_entries += 1;
            
            } else {

                print("bucket is empty, filling bucket\n", .{});
                var bucket = ArrayList(Entry).init(self.allocator);
                try bucket.append(new_entry);
                self.buckets[b_idx] = bucket;
                
                self.n_entries += 1;
            }
            // print("no collision\n", .{});

            // print("putting {s} entry in {}\n", .{key, b_idx});

        }


        pub fn get(self: *Self, key: []const u8) ?Object {

            const h = hash(key);
            
                
            const maybe_bucket = self.buckets[h % self.buckets.len];

            if (maybe_bucket) |bucket| {

                for (bucket.items) |entry| {
                    if (std.mem.eql(u8, entry.key, key)) {
                        return entry.val;
                    } 
                }

            } 

            return null;

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

        // fn checkCollision(self: *Self, key: []const u8) bool{
        //
        //     print("CHECKING collision for {s}\n", .{key});
        //
        //     const b_idx = hash(key) % self.buckets.len;
        //
        //     const maybe_entry = self.buckets[b_idx];
        //
        //     if (maybe_entry) |entry| { 
        //
        //         print("entry = {}\n", .{entry});
        //
        //         // print("bucket is not empty, new key = {s}, old key = {s}\n", .{key, entry.key});
        //
        //         if (std.mem.eql(u8, entry.key, key)) { // no collision
        //             // we want to update value
        //             return false;
        //         }
        //
        //         return true;
        //
        //     } else {
        //         // bucket is empty 
        //         return false; // no collision
        //     }
        //
        //
        // }

        
        fn resize(self: *Self) Allocator.Error!void {
            const new_size = self.buckets.?.len * 2;

            const old_n_entries = self.n_entries;
            
            
            print("\nresizing from {} to {} buckets\n", .{self.buckets.?.len, new_size});
            defer print("resize successfull\n", .{});

            self.printHM();
            
            self.n_entries = 0;

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

            std.debug.assert(self.n_entries == old_n_entries);

            
            self.allocator.free(old_buckets);
            
            self.printHM();

        }

        pub fn printHM(self: *Self) void {
            print("store has {} entries\n", .{self.n_entries});


            for (self.buckets) |maybe_bucket| {

                if (maybe_bucket) |bucket| {
                    
                    for (bucket.items) |entry| {
                        print("key: {s}, val: {}\n", .{entry.key, entry.val});

                    }

                }

            }


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

    var store = try HashMap().init(allocator);
    defer store.deinit();
        
    try store.put(token1.tokenLiteral(), obj1);
    try store.put(token2.tokenLiteral(), obj2);
    try store.put(token3.tokenLiteral(), obj3);

    try expect(store.get(token1.tokenLiteral()).?.integer == obj1.integer);
    try expect(store.n_entries == 3);
    
    store.printHM();

    try store.put(token1.tokenLiteral(), Object {.integer = 100});
    store.printHM();
    
    // try store.put(obj);

    // print("created env store: {any}\n", .{store.buckets.?});

}
