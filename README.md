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



```sh
zig build -Doptimize=ReleaseFast 
poop "go/monkey/monkey_go python_comparisons/factorial.mky"\
                "python3 python_comparisons/factorial.py"\
                "zig-out/bin/monkey python_comparisons/factorial.mky"\

Benchmark 1 (1362 runs): go/monkey/monkey_go python_comparisons/factorial.mky
  measurement          mean ± σ            min … max           outliers         delta
  wall_time          3.67ms ±  521us    3.27ms … 14.5ms        110 ( 8%)        0%
  peak_rss           4.39MB ±  103KB    4.15MB … 4.88MB         37 ( 3%)        0%
Benchmark 2 (215 runs): python3 python_comparisons/factorial.py
  measurement          mean ± σ            min … max           outliers         delta
  wall_time          23.3ms ± 1.65ms    22.4ms … 44.3ms         14 ( 7%)        💩+535.8% ±  3.0%
  peak_rss           11.6MB ±  116KB    11.2MB … 12.0MB         63 (29%)        💩+164.1% ±  0.3%
Benchmark 3 (2516 runs): zig-out/bin/monkey python_comparisons/factorial.mky
  measurement          mean ± σ            min … max           outliers         delta
  wall_time          1.98ms ± 5.08ms    1.71ms …  256ms        234 ( 9%)        ⚡- 45.9% ±  7.4%
  peak_rss           2.03MB ± 2.35KB    2.03MB … 2.13MB          2 ( 0%)        ⚡- 53.7% ±  0.1%
poop "go/monkey/monkey_go python_comparisons/recursice.mky"\
                "python3 python_comparisons/recursice.py"\
                "zig-out/bin/monkey python_comparisons/recursice.mky"\

Benchmark 1 (772 runs): go/monkey/monkey_go python_comparisons/recursice.mky
  measurement          mean ± σ            min … max           outliers         delta
  wall_time          6.47ms ± 1.03ms    5.84ms … 19.3ms         66 ( 9%)        0%
  peak_rss           11.1MB ±  106KB    10.8MB … 11.5MB          6 ( 1%)        0%
Benchmark 2 (192 runs): python3 python_comparisons/recursice.py
  measurement          mean ± σ            min … max           outliers         delta
  wall_time          26.1ms ± 25.7ms    22.4ms …  380ms         10 ( 5%)        💩+303.0% ± 28.1%
  peak_rss           11.8MB ±  191KB    11.5MB … 12.5MB         81 (42%)        💩+  6.5% ±  0.2%
Benchmark 3 (458 runs): zig-out/bin/monkey python_comparisons/recursice.mky
  measurement          mean ± σ            min … max           outliers         delta
  wall_time          10.9ms ±  397us    10.4ms … 13.9ms         29 ( 6%)        💩+ 68.9% ±  1.5%
  peak_rss           6.00MB ± 2.30KB    6.00MB … 6.05MB          1 ( 0%)        ⚡- 45.8% ±  0.1%
```
