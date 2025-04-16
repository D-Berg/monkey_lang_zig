debug:
	zig build -Doptimize=Debug

safe: 
	zig build -Doptimize=ReleaseSafe

fast:
	zig build -Doptimize=ReleaseFast 

bench: fast
	poop "python3 python_comparisons/factorial.py" "zig-out/bin/monkey python_comparisons/factorial.mky"
	poop "python3 python_comparisons/recursice.py" "zig-out/bin/monkey python_comparisons/recursice.mky"

