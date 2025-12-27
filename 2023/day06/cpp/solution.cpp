#include <cmath>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

struct Race {
    int64_t time;
    int64_t distance;
};

std::vector<Race> parse_races(const std::string& input) {
    std::istringstream iss(input);
    std::string time_line, distance_line;
    std::getline(iss, time_line);
    std::getline(iss, distance_line);

    std::vector<Race> races;
    std::vector<int64_t> times, distances;

    // Parse times (skip "Time:" label)
    std::istringstream time_stream(time_line.substr(time_line.find(':') + 1));
    int64_t val;
    while (time_stream >> val) {
        times.push_back(val);
    }

    // Parse distances (skip "Distance:" label)
    std::istringstream dist_stream(distance_line.substr(distance_line.find(':') + 1));
    while (dist_stream >> val) {
        distances.push_back(val);
    }

    for (size_t i = 0; i < times.size(); ++i) {
        races.push_back({times[i], distances[i]});
    }

    return races;
}

int64_t count_ways_to_win(int64_t time, int64_t record) {
    /*
     * Count the number of ways to beat the record.
     *
     * If we hold the button for t ms, we travel t * (time - t) mm.
     * We need: t * (time - t) > record
     * Solving: -t^2 + time*t - record > 0
     * Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2
     */
    double discriminant = static_cast<double>(time) * time - 4.0 * record;
    if (discriminant <= 0) {
        return 0;
    }

    double sqrt_d = std::sqrt(discriminant);
    double t_low = (time - sqrt_d) / 2.0;
    double t_high = (time + sqrt_d) / 2.0;

    // We need integer values strictly between the roots
    int64_t first = static_cast<int64_t>(std::floor(t_low)) + 1;
    int64_t last = static_cast<int64_t>(std::ceil(t_high)) - 1;

    if (last < first) {
        return 0;
    }
    return last - first + 1;
}

int64_t part1(const std::vector<Race>& races) {
    int64_t result = 1;
    for (const auto& race : races) {
        int64_t ways = count_ways_to_win(race.time, race.distance);
        result *= ways;
    }
    return result;
}

int64_t part2(const std::vector<Race>& races) {
    // Concatenate all times and distances into single numbers
    std::string time_str, distance_str;
    for (const auto& race : races) {
        time_str += std::to_string(race.time);
        distance_str += std::to_string(race.distance);
    }

    int64_t time = std::stoll(time_str);
    int64_t record = std::stoll(distance_str);

    return count_ways_to_win(time, record);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Cannot open ../input.txt" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string input = buffer.str();

    std::vector<Race> races = parse_races(input);

    std::cout << "Part 1: " << part1(races) << std::endl;
    std::cout << "Part 2: " << part2(races) << std::endl;

    return 0;
}
