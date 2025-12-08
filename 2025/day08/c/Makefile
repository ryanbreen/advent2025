CC = gcc
CFLAGS = -O2 -Wall -Wextra -std=c11
TARGET = solution

all: $(TARGET)

$(TARGET): solution.c
	$(CC) $(CFLAGS) -o $(TARGET) solution.c -lm

clean:
	rm -f $(TARGET)

.PHONY: all clean
