# Monkey Lang interpreter in zig

## Build

```
zig build -Doptimize=ReleaseFast
```

## Test

Test one with:
```
zig test src/filename.zig
```

Test all with:
```
zig build test # &> /dev/null supress errors for nicer output
```

