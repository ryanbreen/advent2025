# Makefile for Day 10 ARM64 Assembly solutions

.PHONY: all clean test solution rational matrix_example

all: solution rational matrix_example

solution: solution.s rational.s
	as rational.s -o rational_sol.o
	as solution.s -o solution.o
	ld solution.o rational_sol.o -o solution -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64

rational: rational.s
	as rational.s -o rational.o
	ld rational.o -o rational -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _test_rational_main -arch arm64

matrix_example: matrix_example.s rational.s
	as rational.s -o rational_lib.o
	as matrix_example.s -o matrix_example.o
	ld matrix_example.o rational_lib.o -o matrix_example -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _matrix_example_main -arch arm64

test: rational matrix_example
	@echo "=== Running rational arithmetic library tests ==="
	./rational
	@echo ""
	@echo "=== Running matrix example ==="
	./matrix_example

clean:
	rm -f *.o solution rational matrix_example

run: solution
	./solution
