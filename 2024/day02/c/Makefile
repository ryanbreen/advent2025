CC = gcc
CFLAGS = -O3 -Wall -Wextra
TARGET = solution

all: $(TARGET)

$(TARGET): solution.c
	$(CC) $(CFLAGS) -o $(TARGET) solution.c

clean:
	rm -f $(TARGET)

.PHONY: all clean
