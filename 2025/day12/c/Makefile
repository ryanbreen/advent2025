CC = gcc
CFLAGS = -std=c11 -O2 -Wall -Wextra -Wpedantic
TARGET = solution

.PHONY: all clean test

all: $(TARGET)

$(TARGET): solution.c
	$(CC) $(CFLAGS) -o $(TARGET) solution.c

clean:
	rm -f $(TARGET)

test: $(TARGET)
	./$(TARGET)
