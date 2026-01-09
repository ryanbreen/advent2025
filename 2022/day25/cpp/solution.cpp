#include <iostream>
#include <fstream>
#include <string>
#include <vector>

// Convert a SNAFU digit to its decimal value
int snafuDigitToDecimal(char c) {
    switch (c) {
        case '2': return 2;
        case '1': return 1;
        case '0': return 0;
        case '-': return -1;
        case '=': return -2;
        default: return 0;
    }
}

// Convert a SNAFU number string to decimal
long long snafuToDecimal(const std::string& s) {
    long long result = 0;
    for (char c : s) {
        result = result * 5 + snafuDigitToDecimal(c);
    }
    return result;
}

// Convert a decimal number to SNAFU string
std::string decimalToSnafu(long long n) {
    if (n == 0) {
        return "0";
    }

    std::string digits;
    while (n != 0) {
        int remainder = n % 5;
        if (remainder <= 2) {
            digits += ('0' + remainder);
            n /= 5;
        } else if (remainder == 3) {
            digits += '=';
            n = n / 5 + 1;
        } else { // remainder == 4
            digits += '-';
            n = n / 5 + 1;
        }
    }

    // Reverse the string
    std::string result;
    for (int i = digits.size() - 1; i >= 0; i--) {
        result += digits[i];
    }
    return result;
}

std::string part1(const std::vector<std::string>& lines) {
    long long total = 0;
    for (const auto& line : lines) {
        total += snafuToDecimal(line);
    }
    return decimalToSnafu(total);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }
    file.close();

    std::cout << "Part 1: " << part1(lines) << std::endl;
    std::cout << "Part 2: No Part 2 on Day 25!" << std::endl;

    return 0;
}
