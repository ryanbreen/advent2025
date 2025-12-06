#include <iostream>
#include <fstream>
#include <string>
#include <string_view>
#include <array>
#include <optional>
#include <cctype>
#include <cstdlib>

/* Mapping of spelled-out digits to their numeric value */
struct WordDigit {
    std::string_view word;
    char digit;
};

constexpr std::array<WordDigit, 9> WORD_DIGITS = {{
    {"one",   '1'},
    {"two",   '2'},
    {"three", '3'},
    {"four",  '4'},
    {"five",  '5'},
    {"six",   '6'},
    {"seven", '7'},
    {"eight", '8'},
    {"nine",  '9'}
}};

/* Check if a spelled-out digit starts at the given position */
std::optional<char> find_word_digit(std::string_view str) {
    for (const auto& [word, digit] : WORD_DIGITS) {
        if (str.starts_with(word)) {
            return digit;
        }
    }
    return std::nullopt;
}

/* Extract calibration value from a line (first digit * 10 + last digit) */
int extract_calibration_value(std::string_view line, bool include_words) {
    std::optional<char> first;
    std::optional<char> last;

    for (size_t i = 0; i < line.length(); ++i) {
        std::optional<char> digit;

        if (std::isdigit(static_cast<unsigned char>(line[i]))) {
            digit = line[i];
        } else if (include_words) {
            digit = find_word_digit(line.substr(i));
        }

        if (digit) {
            if (!first) {
                first = digit;
            }
            last = digit;
        }
    }

    if (first && last) {
        return (*first - '0') * 10 + (*last - '0');
    }

    return 0;
}

/* Process input file and sum calibration values */
int solve(const std::string& filename, bool include_words) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Unable to open file '" << filename << "'\n";
        return -1;
    }

    int total = 0;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            total += extract_calibration_value(line, include_words);
        }
    }

    return total;
}

int part1(const std::string& filename) {
    return solve(filename, false);
}

int part2(const std::string& filename) {
    return solve(filename, true);
}

int main() {
    const std::string input_file = "../input.txt";

    const int result1 = part1(input_file);
    if (result1 < 0) {
        return EXIT_FAILURE;
    }
    std::cout << "Part 1: " << result1 << '\n';

    const int result2 = part2(input_file);
    if (result2 < 0) {
        return EXIT_FAILURE;
    }
    std::cout << "Part 2: " << result2 << '\n';

    return EXIT_SUCCESS;
}
