#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <climits>

std::unordered_map<std::string, long long> parse_filesystem(const std::vector<std::string>& lines) {
    std::vector<std::string> path;
    std::unordered_map<std::string, long long> dir_sizes;

    for (const auto& line : lines) {
        if (line.rfind("$ cd", 0) == 0) {
            std::string target = line.substr(5);
            if (target == "/") {
                path.clear();
                path.push_back("/");
            } else if (target == "..") {
                if (!path.empty()) {
                    path.pop_back();
                }
            } else {
                path.push_back(target);
            }
        } else if (line.rfind("$ ls", 0) == 0) {
            continue;
        } else if (line.rfind("dir ", 0) == 0) {
            continue;
        } else {
            // It's a file with size
            std::istringstream iss(line);
            long long size;
            iss >> size;

            // Add size to current directory and all parent directories
            std::string dir_path;
            for (size_t i = 0; i < path.size(); ++i) {
                if (i == 0) {
                    dir_path = "/";
                } else {
                    dir_path += "/" + path[i];
                }
                dir_sizes[dir_path] += size;
            }
        }
    }

    return dir_sizes;
}

long long part1(const std::unordered_map<std::string, long long>& dir_sizes) {
    long long sum = 0;
    for (const auto& [path, size] : dir_sizes) {
        if (size <= 100000) {
            sum += size;
        }
    }
    return sum;
}

long long part2(const std::unordered_map<std::string, long long>& dir_sizes) {
    const long long total_space = 70000000;
    const long long needed_space = 30000000;
    long long used_space = dir_sizes.at("/");
    long long free_space = total_space - used_space;
    long long need_to_free = needed_space - free_space;

    long long smallest = LLONG_MAX;
    for (const auto& [path, size] : dir_sizes) {
        if (size >= need_to_free && size < smallest) {
            smallest = size;
        }
    }
    return smallest;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    auto dir_sizes = parse_filesystem(lines);

    std::cout << "Part 1: " << part1(dir_sizes) << std::endl;
    std::cout << "Part 2: " << part2(dir_sizes) << std::endl;

    return 0;
}
