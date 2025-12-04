CXX = g++
CXXFLAGS = -O3 -Wall -Wextra -std=c++17
TARGET = solution

all: $(TARGET)

$(TARGET): solution.cpp
	$(CXX) $(CXXFLAGS) -o $(TARGET) solution.cpp

clean:
	rm -f $(TARGET)

.PHONY: all clean
