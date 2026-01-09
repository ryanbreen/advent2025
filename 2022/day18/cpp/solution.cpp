#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <set>
#include <queue>
#include <tuple>
#include <array>
#include <algorithm>
#include <climits>

using Coord = std::tuple<int, int, int>;

// 6 directions: +x, -x, +y, -y, +z, -z
const std::array<Coord, 6> DIRECTIONS = {{
    {1, 0, 0}, {-1, 0, 0},
    {0, 1, 0}, {0, -1, 0},
    {0, 0, 1}, {0, 0, -1}
}};

std::set<Coord> parseInput(const std::string& text) {
    std::set<Coord> cubes;
    std::istringstream stream(text);
    std::string line;

    while (std::getline(stream, line)) {
        if (line.empty()) continue;

        int x, y, z;
        char comma;
        std::istringstream lineStream(line);
        lineStream >> x >> comma >> y >> comma >> z;
        cubes.insert({x, y, z});
    }

    return cubes;
}

int part1(const std::set<Coord>& cubes) {
    int surfaceArea = 0;

    for (const auto& [x, y, z] : cubes) {
        for (const auto& [dx, dy, dz] : DIRECTIONS) {
            if (cubes.find({x + dx, y + dy, z + dz}) == cubes.end()) {
                surfaceArea++;
            }
        }
    }

    return surfaceArea;
}

int part2(const std::set<Coord>& cubes) {
    // Find bounding box with 1 unit padding
    int minX = INT_MAX, maxX = INT_MIN;
    int minY = INT_MAX, maxY = INT_MIN;
    int minZ = INT_MAX, maxZ = INT_MIN;

    for (const auto& [x, y, z] : cubes) {
        minX = std::min(minX, x);
        maxX = std::max(maxX, x);
        minY = std::min(minY, y);
        maxY = std::max(maxY, y);
        minZ = std::min(minZ, z);
        maxZ = std::max(maxZ, z);
    }

    minX--; maxX++;
    minY--; maxY++;
    minZ--; maxZ++;

    // BFS to find all exterior air cells
    std::set<Coord> exterior;
    std::queue<Coord> queue;

    Coord start = {minX, minY, minZ};
    queue.push(start);
    exterior.insert(start);

    while (!queue.empty()) {
        auto [x, y, z] = queue.front();
        queue.pop();

        for (const auto& [dx, dy, dz] : DIRECTIONS) {
            int nx = x + dx;
            int ny = y + dy;
            int nz = z + dz;

            // Stay within bounds
            if (nx < minX || nx > maxX || ny < minY || ny > maxY || nz < minZ || nz > maxZ) {
                continue;
            }

            Coord next = {nx, ny, nz};

            // Skip cubes and already visited
            if (cubes.find(next) != cubes.end() || exterior.find(next) != exterior.end()) {
                continue;
            }

            exterior.insert(next);
            queue.push(next);
        }
    }

    // Count faces touching exterior air
    int surfaceArea = 0;
    for (const auto& [x, y, z] : cubes) {
        for (const auto& [dx, dy, dz] : DIRECTIONS) {
            if (exterior.find({x + dx, y + dy, z + dz}) != exterior.end()) {
                surfaceArea++;
            }
        }
    }

    return surfaceArea;
}

int main() {
    // Read input file
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();
    file.close();

    std::set<Coord> cubes = parseInput(text);

    std::cout << "Part 1: " << part1(cubes) << std::endl;
    std::cout << "Part 2: " << part2(cubes) << std::endl;

    return 0;
}
