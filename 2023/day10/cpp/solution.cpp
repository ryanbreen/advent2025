#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <map>
#include <set>
#include <utility>

using namespace std;

// Direction offsets: N, S, W, E
const vector<pair<int, int>> DIRECTIONS = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

// Pipe connections: each pipe type maps to which directions it connects
map<char, vector<pair<int, int>>> getPipeConnections() {
    return {
        {'|', {{-1, 0}, {1, 0}}},   // N, S
        {'-', {{0, -1}, {0, 1}}},   // W, E
        {'L', {{-1, 0}, {0, 1}}},   // N, E
        {'J', {{-1, 0}, {0, -1}}},  // N, W
        {'7', {{1, 0}, {0, -1}}},   // S, W
        {'F', {{1, 0}, {0, 1}}}     // S, E
    };
}

pair<int, int> findStart(const vector<string>& grid) {
    for (int r = 0; r < (int)grid.size(); r++) {
        for (int c = 0; c < (int)grid[r].size(); c++) {
            if (grid[r][c] == 'S') {
                return {r, c};
            }
        }
    }
    return {-1, -1};
}

vector<pair<int, int>> getNeighbors(const vector<string>& grid, pair<int, int> pos,
                                     const map<char, vector<pair<int, int>>>& pipeConnections) {
    int r = pos.first;
    int c = pos.second;
    int rows = grid.size();
    int cols = grid[0].size();
    char ch = grid[r][c];
    vector<pair<int, int>> neighbors;

    if (ch == 'S') {
        // S can connect to any adjacent pipe that connects back to it
        for (const auto& [dr, dc] : DIRECTIONS) {
            int nr = r + dr;
            int nc = c + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                char adjCh = grid[nr][nc];
                if (pipeConnections.count(adjCh)) {
                    // Check if adjacent pipe connects back to S
                    for (const auto& [adjDr, adjDc] : pipeConnections.at(adjCh)) {
                        if (nr + adjDr == r && nc + adjDc == c) {
                            neighbors.push_back({nr, nc});
                            break;
                        }
                    }
                }
            }
        }
    } else if (pipeConnections.count(ch)) {
        for (const auto& [dr, dc] : pipeConnections.at(ch)) {
            int nr = r + dr;
            int nc = c + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                neighbors.push_back({nr, nc});
            }
        }
    }

    return neighbors;
}

map<pair<int, int>, int> findLoop(const vector<string>& grid, pair<int, int> start,
                                   const map<char, vector<pair<int, int>>>& pipeConnections) {
    map<pair<int, int>, int> distances;
    queue<pair<int, int>> q;

    distances[start] = 0;
    q.push(start);

    while (!q.empty()) {
        auto pos = q.front();
        q.pop();

        for (const auto& neighbor : getNeighbors(grid, pos, pipeConnections)) {
            if (distances.find(neighbor) == distances.end()) {
                distances[neighbor] = distances[pos] + 1;
                q.push(neighbor);
            }
        }
    }

    return distances;
}

char determineStartPipe(const vector<string>& grid, pair<int, int> start,
                        const set<pair<int, int>>& loopPositions,
                        const map<char, vector<pair<int, int>>>& pipeConnections) {
    int r = start.first;
    int c = start.second;
    int rows = grid.size();
    int cols = grid[0].size();

    set<pair<int, int>> connections;
    for (const auto& [dr, dc] : DIRECTIONS) {
        int nr = r + dr;
        int nc = c + dc;
        if (loopPositions.count({nr, nc})) {
            char adjCh = grid[nr][nc];
            if (pipeConnections.count(adjCh)) {
                // Check if this pipe connects back to S
                for (const auto& [adjDr, adjDc] : pipeConnections.at(adjCh)) {
                    if (nr + adjDr == r && nc + adjDc == c) {
                        connections.insert({dr, dc});
                        break;
                    }
                }
            }
        }
    }

    for (const auto& [pipe, dirs] : pipeConnections) {
        set<pair<int, int>> pipeDirections(dirs.begin(), dirs.end());
        if (pipeDirections == connections) {
            return pipe;
        }
    }

    return 'S';
}

int part1(const vector<string>& grid) {
    auto pipeConnections = getPipeConnections();
    auto start = findStart(grid);
    auto distances = findLoop(grid, start, pipeConnections);

    int maxDist = 0;
    for (const auto& [pos, dist] : distances) {
        maxDist = max(maxDist, dist);
    }
    return maxDist;
}

int part2(const vector<string>& grid) {
    auto pipeConnections = getPipeConnections();
    auto start = findStart(grid);
    auto distances = findLoop(grid, start, pipeConnections);

    set<pair<int, int>> loopPositions;
    for (const auto& [pos, dist] : distances) {
        loopPositions.insert(pos);
    }

    // Replace S with its actual pipe type
    char startPipe = determineStartPipe(grid, start, loopPositions, pipeConnections);
    vector<string> modifiedGrid = grid;
    modifiedGrid[start.first][start.second] = startPipe;

    int rows = modifiedGrid.size();
    int cols = modifiedGrid[0].size();
    int enclosed = 0;

    for (int r = 0; r < rows; r++) {
        bool inside = false;
        for (int c = 0; c < cols; c++) {
            if (loopPositions.count({r, c})) {
                char ch = modifiedGrid[r][c];
                // Count vertical crossings: pipes that have a north connection
                if (ch == '|' || ch == 'L' || ch == 'J') {
                    inside = !inside;
                }
            } else {
                if (inside) {
                    enclosed++;
                }
            }
        }
    }

    return enclosed;
}

int main() {
    ifstream inputFile("../input.txt");
    if (!inputFile) {
        cerr << "Error: Cannot open input file" << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    while (getline(inputFile, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    inputFile.close();

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
