CC = gcc
CFLAGS = -Wall -Wextra -O2 -std=c11
TARGET = solution

all: $(TARGET)

$(TARGET): solution.c
	$(CC) $(CFLAGS) -o $(TARGET) solution.c

clean:
	rm -f $(TARGET)

.PHONY: all clean
