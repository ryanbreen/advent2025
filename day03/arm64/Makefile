# Makefile for ARM64 Assembly Solution
# macOS ARM64 (Apple Silicon)

AS = as
LD = ld
RM = rm -f

TARGET = solution
SRC = solution.s

# macOS ARM64 linker flags
LDFLAGS = -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main -arch arm64

all: $(TARGET)

$(TARGET): $(SRC)
	$(AS) -arch arm64 -o $(TARGET).o $(SRC)
	$(LD) $(LDFLAGS) -o $(TARGET) $(TARGET).o

clean:
	$(RM) $(TARGET) $(TARGET).o

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
