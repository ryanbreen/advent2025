#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <regex>
#include <set>
#include <algorithm>
#include <tuple>

const int WIDTH = 101;
const int HEIGHT = 103;

struct Robot {
    int px, py, vx, vy;
};

std::vector<Robot> parseRobots(const std::string& text) {
    std::vector<Robot> robots;
    std::regex pattern(R"(p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+))");
    std::smatch match;

    std::istringstream stream(text);
    std::string line;

    while (std::getline(stream, line)) {
        if (std::regex_search(line, match, pattern)) {
            Robot r;
            r.px = std::stoi(match[1]);
            r.py = std::stoi(match[2]);
            r.vx = std::stoi(match[3]);
            r.vy = std::stoi(match[4]);
            robots.push_back(r);
        }
    }

    return robots;
}

std::vector<std::pair<int, int>> simulate(const std::vector<Robot>& robots, int seconds) {
    std::vector<std::pair<int, int>> positions;

    for (const auto& robot : robots) {
        // Position after 'seconds' time, with wrapping
        int new_x = ((robot.px + robot.vx * seconds) % WIDTH + WIDTH) % WIDTH;
        int new_y = ((robot.py + robot.vy * seconds) % HEIGHT + HEIGHT) % HEIGHT;
        positions.push_back({new_x, new_y});
    }

    return positions;
}

std::tuple<int, int, int, int> countQuadrants(const std::vector<std::pair<int, int>>& positions) {
    int mid_x = WIDTH / 2;   // 50
    int mid_y = HEIGHT / 2;  // 51

    int q1 = 0, q2 = 0, q3 = 0, q4 = 0;

    for (const auto& [x, y] : positions) {
        if (x == mid_x || y == mid_y) {
            continue;  // Skip robots on middle lines
        }

        if (x < mid_x && y < mid_y) {
            q1++;  // Top-left
        } else if (x > mid_x && y < mid_y) {
            q2++;  // Top-right
        } else if (x < mid_x && y > mid_y) {
            q3++;  // Bottom-left
        } else {
            q4++;  // Bottom-right
        }
    }

    return {q1, q2, q3, q4};
}

long long part1(const std::vector<Robot>& robots) {
    auto positions = simulate(robots, 100);
    auto [q1, q2, q3, q4] = countQuadrants(positions);
    return static_cast<long long>(q1) * q2 * q3 * q4;
}

int part2(const std::vector<Robot>& robots) {
    // The Christmas tree appears when robots cluster together
    // Look for a frame with a long horizontal line of robots (tree base/border)
    for (int seconds = 1; seconds <= WIDTH * HEIGHT; seconds++) {
        auto positions = simulate(robots, seconds);
        std::set<std::pair<int, int>> pos_set(positions.begin(), positions.end());

        // Look for a horizontal line of at least 20 consecutive robots
        for (int y = 0; y < HEIGHT; y++) {
            int max_consecutive = 0;
            int consecutive = 0;

            for (int x = 0; x < WIDTH; x++) {
                if (pos_set.count({x, y})) {
                    consecutive++;
                    max_consecutive = std::max(max_consecutive, consecutive);
                } else {
                    consecutive = 0;
                }
            }

            if (max_consecutive >= 20) {
                return seconds;
            }
        }
    }

    return -1;
}

int main() {
    // Read input file
    std::ifstream file("../input.txt");
    std::string input((std::istreambuf_iterator<char>(file)),
                      std::istreambuf_iterator<char>());

    // Parse robots
    auto robots = parseRobots(input);

    // Part 1
    std::cout << "Part 1: " << part1(robots) << std::endl;

    // Part 2
    std::cout << "Part 2: " << part2(robots) << std::endl;

    return 0;
}
