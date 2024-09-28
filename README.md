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

## Benchmark

```
hyperfine "python3 python_comparisons/recursice.py" "zig build run -Doptimize=ReleaseFast -- python_comparisons/recursice.mky"
```

As of now python is about 5 times faster. 
