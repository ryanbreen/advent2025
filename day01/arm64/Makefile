SDK_PATH := $(shell xcrun -sdk macosx --show-sdk-path)

solution: solution.o
	ld -o solution solution.o -lSystem -syslibroot $(SDK_PATH) -e _main -arch arm64

solution.o: solution.s
	as -arch arm64 -o solution.o solution.s

clean:
	rm -f solution.o solution

run: solution
	./solution

.PHONY: clean run
