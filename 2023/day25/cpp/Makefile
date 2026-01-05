CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra
TARGET = solution

all: $(TARGET)

$(TARGET): solution.cpp
	$(CXX) $(CXXFLAGS) -o $(TARGET) solution.cpp

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
