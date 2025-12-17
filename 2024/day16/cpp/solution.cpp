#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <limits>
#include <tuple>

using namespace std;

// Directions: 0=East, 1=South, 2=West, 3=North
const int DX[] = {1, 0, -1, 0};
const int DY[] = {0, 1, 0, -1};

// Hash function for tuple<int, int, int>
struct StateHash {
    size_t operator()(const tuple<int, int, int>& t) const {
        auto h1 = hash<int>{}(get<0>(t));
        auto h2 = hash<int>{}(get<1>(t));
        auto h3 = hash<int>{}(get<2>(t));
        return h1 ^ (h2 << 1) ^ (h3 << 2);
    }
};

// Hash function for pair<int, int>
struct PairHash {
    size_t operator()(const pair<int, int>& p) const {
        return hash<int>{}(p.first) ^ (hash<int>{}(p.second) << 1);
    }
};

struct State {
    int cost;
    int x;
    int y;
    int dir;

    bool operator>(const State& other) const {
        return cost > other.cost;
    }
};

pair<vector<string>, tuple<int, int, int, int>> parse_input(const string& filename) {
    ifstream file(filename);
    vector<string> grid;
    string line;
    int start_x = -1, start_y = -1;
    int end_x = -1, end_y = -1;

    int y = 0;
    while (getline(file, line)) {
        grid.push_back(line);
        for (int x = 0; x < line.length(); x++) {
            if (line[x] == 'S') {
                start_x = x;
                start_y = y;
            } else if (line[x] == 'E') {
                end_x = x;
                end_y = y;
            }
        }
        y++;
    }

    return {grid, {start_x, start_y, end_x, end_y}};
}

unordered_map<tuple<int, int, int>, int, StateHash> dijkstra_forward(
    const vector<string>& grid, int start_x, int start_y) {

    priority_queue<State, vector<State>, greater<State>> pq;
    unordered_map<tuple<int, int, int>, int, StateHash> dist;

    // Start facing East (direction 0)
    pq.push({0, start_x, start_y, 0});

    int height = grid.size();
    int width = grid[0].size();

    while (!pq.empty()) {
        State current = pq.top();
        pq.pop();

        auto state = make_tuple(current.x, current.y, current.dir);
        if (dist.find(state) != dist.end()) {
            continue;
        }
        dist[state] = current.cost;

        // Move forward
        int nx = current.x + DX[current.dir];
        int ny = current.y + DY[current.dir];
        if (ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] != '#') {
            pq.push({current.cost + 1, nx, ny, current.dir});
        }

        // Turn left
        int left_dir = (current.dir - 1 + 4) % 4;
        pq.push({current.cost + 1000, current.x, current.y, left_dir});

        // Turn right
        int right_dir = (current.dir + 1) % 4;
        pq.push({current.cost + 1000, current.x, current.y, right_dir});
    }

    return dist;
}

unordered_map<tuple<int, int, int>, int, StateHash> dijkstra_backward(
    const vector<string>& grid, int end_x, int end_y) {

    priority_queue<State, vector<State>, greater<State>> pq;
    unordered_map<tuple<int, int, int>, int, StateHash> dist;

    // At end, we can arrive facing any direction
    for (int d = 0; d < 4; d++) {
        pq.push({0, end_x, end_y, d});
    }

    int height = grid.size();
    int width = grid[0].size();

    while (!pq.empty()) {
        State current = pq.top();
        pq.pop();

        auto state = make_tuple(current.x, current.y, current.dir);
        if (dist.find(state) != dist.end()) {
            continue;
        }
        dist[state] = current.cost;

        // Reverse of "move forward": come from behind
        int px = current.x - DX[current.dir];
        int py = current.y - DY[current.dir];
        if (py >= 0 && py < height && px >= 0 && px < width && grid[py][px] != '#') {
            pq.push({current.cost + 1, px, py, current.dir});
        }

        // Reverse of turn: came from same position with different direction
        int left_dir = (current.dir - 1 + 4) % 4;
        pq.push({current.cost + 1000, current.x, current.y, left_dir});

        int right_dir = (current.dir + 1) % 4;
        pq.push({current.cost + 1000, current.x, current.y, right_dir});
    }

    return dist;
}

int part1(const vector<string>& grid, int start_x, int start_y, int end_x, int end_y) {
    auto dist = dijkstra_forward(grid, start_x, start_y);

    int min_cost = numeric_limits<int>::max();
    for (int d = 0; d < 4; d++) {
        auto state = make_tuple(end_x, end_y, d);
        if (dist.find(state) != dist.end()) {
            min_cost = min(min_cost, dist[state]);
        }
    }

    return min_cost;
}

int part2(const vector<string>& grid, int start_x, int start_y,
          int end_x, int end_y, int best_score) {

    auto dist_from_start = dijkstra_forward(grid, start_x, start_y);
    auto dist_to_end = dijkstra_backward(grid, end_x, end_y);

    unordered_set<pair<int, int>, PairHash> tiles_on_best_path;

    int height = grid.size();
    int width = grid[0].size();

    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (grid[y][x] == '#') {
                continue;
            }

            // Check if this tile is on any optimal path
            for (int d = 0; d < 4; d++) {
                auto state = make_tuple(x, y, d);

                int from_start = numeric_limits<int>::max();
                if (dist_from_start.find(state) != dist_from_start.end()) {
                    from_start = dist_from_start[state];
                }

                int to_end = numeric_limits<int>::max();
                if (dist_to_end.find(state) != dist_to_end.end()) {
                    to_end = dist_to_end[state];
                }

                if (from_start != numeric_limits<int>::max() &&
                    to_end != numeric_limits<int>::max() &&
                    from_start + to_end == best_score) {
                    tiles_on_best_path.insert({x, y});
                    break;
                }
            }
        }
    }

    return tiles_on_best_path.size();
}

int main() {
    auto [grid, coords] = parse_input("../input.txt");
    auto [start_x, start_y, end_x, end_y] = coords;

    int answer1 = part1(grid, start_x, start_y, end_x, end_y);
    cout << "Part 1: " << answer1 << endl;

    int answer2 = part2(grid, start_x, start_y, end_x, end_y, answer1);
    cout << "Part 2: " << answer2 << endl;

    return 0;
}
