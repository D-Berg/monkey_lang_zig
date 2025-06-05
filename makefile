.ONESHELL:

debug:
	zig build -Doptimize=Debug

safe: 
	zig build -Doptimize=ReleaseSafe

fast:
	zig build -Doptimize=ReleaseFast 

go-build:
	cd go_implementation/monkey
	go mod tidy
	go build -o zig-out/bin/monkey_go go_implementation/monkey/main.go


bench: fast
	poop "go/monkey/monkey_go python_comparisons/factorial.mky"\
		"python3 python_comparisons/factorial.py"\
		"zig-out/bin/monkey python_comparisons/factorial.mky"\
		"zig-out/main/bin/monkey python_comparisons/factorial.mky"\
		
	poop "go/monkey/monkey_go python_comparisons/recursice.mky"\
		"python3 python_comparisons/recursice.py"\
		"zig-out/bin/monkey python_comparisons/recursice.mky"\
		"zig-out/main/bin/monkey python_comparisons/recursice.mky"


bench-self: fast
	poop "zig-out/main/bin/monkey python_comparisons/factorial.mky"\
		"zig-out/bin/monkey python_comparisons/factorial.mky"\
		
	poop "zig-out/main/bin/monkey python_comparisons/recursice.mky"\
		"zig-out/bin/monkey python_comparisons/recursice.mky"\
