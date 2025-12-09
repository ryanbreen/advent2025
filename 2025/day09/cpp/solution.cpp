#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>

using namespace std;

struct Point {
    int x, y;
};

struct HorizontalEdge {
    int y, x_min, x_max;
};

struct VerticalEdge {
    int x, y_min, y_max;
};

vector<Point> points;
vector<HorizontalEdge> horizontal_edges;
vector<VerticalEdge> vertical_edges;

void parse_input(const string& filename) {
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (line.empty()) continue;
        size_t comma = line.find(',');
        int x = stoi(line.substr(0, comma));
        int y = stoi(line.substr(comma + 1));
        points.push_back({x, y});
    }
}

long long part1() {
    long long max_area = 0;
    int n = points.size();

    // Check all pairs of points as opposite corners
    for (int i = 0; i < n; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < n; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            // Rectangle area = width * height (inclusive of both corners)
            long long width = abs(x2 - x1) + 1;
            long long height = abs(y2 - y1) + 1;
            long long area = width * height;

            if (area > max_area) max_area = area;
        }
    }

    return max_area;
}

void build_edges() {
    int n = points.size();
    horizontal_edges.clear();
    vertical_edges.clear();
    horizontal_edges.reserve(n);
    vertical_edges.reserve(n);

    for (int i = 0; i < n; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;
        int x2 = points[(i + 1) % n].x;
        int y2 = points[(i + 1) % n].y;

        if (y1 == y2) {
            // Horizontal edge
            horizontal_edges.push_back({y1, min(x1, x2), max(x1, x2)});
        } else {
            // Vertical edge
            vertical_edges.push_back({x1, min(y1, y2), max(y1, y2)});
        }
    }

    // Sort vertical edges by x for potentially faster early exit
    sort(vertical_edges.begin(), vertical_edges.end(),
         [](const VerticalEdge& a, const VerticalEdge& b) {
             return a.x < b.x;
         });
}

bool is_inside_polygon(double x, double y) {
    double crossings = 0.0;

    // Cast ray to the right
    for (const auto& edge : vertical_edges) {
        if (edge.x <= x) continue;

        if (edge.y_min < y && y < edge.y_max) {
            crossings += 1.0;
        } else if (y == edge.y_min || y == edge.y_max) {
            crossings += 0.5;
        }
    }

    double remainder = fmod(crossings, 2.0);
    return fabs(remainder - 1.0) < 0.01;
}

bool rectangle_valid(int x1, int y1, int x2, int y2) {
    int min_x = min(x1, x2);
    int max_x = max(x1, x2);
    int min_y = min(y1, y2);
    int max_y = max(y1, y2);

    // Check if any vertical edge crosses through rectangle interior
    for (const auto& edge : vertical_edges) {
        if (min_x < edge.x && edge.x < max_x) {
            if (!(edge.y_max <= min_y || edge.y_min >= max_y)) {
                return false;
            }
        }
    }

    // Check if any horizontal edge crosses through rectangle interior
    for (const auto& edge : horizontal_edges) {
        if (min_y < edge.y && edge.y < max_y) {
            if (!(edge.x_max <= min_x || edge.x_min >= max_x)) {
                return false;
            }
        }
    }

    // Check center point is inside polygon
    double center_x = (min_x + max_x) / 2.0;
    double center_y = (min_y + max_y) / 2.0;
    return is_inside_polygon(center_x, center_y);
}

long long part2() {
    build_edges();

    long long max_area = 0;
    int n = points.size();

    for (int i = 0; i < n; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < n; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            if (rectangle_valid(x1, y1, x2, y2)) {
                long long width = abs(x2 - x1) + 1;
                long long height = abs(y2 - y1) + 1;
                long long area = width * height;

                if (area > max_area) max_area = area;
            }
        }
    }

    return max_area;
}

int main() {
    parse_input("../input.txt");

    cout << "Part 1: " << part1() << endl;
    cout << "Part 2: " << part2() << endl;

    return 0;
}
