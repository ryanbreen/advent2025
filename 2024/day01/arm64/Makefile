.PHONY: all clean

all: solution

solution: solution.s
	as -o solution.o solution.s
	ld -o solution solution.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main -arch arm64

clean:
	rm -f solution.o solution
