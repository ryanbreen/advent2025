.PHONY: all clean run

all: solution

solution: solution.s
	as -o solution.o solution.s
	ld -o solution solution.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main -arch arm64

run: solution
	./solution

clean:
	rm -f solution solution.o
