#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <utility>
#include <algorithm>

struct Region {
    int width;
    int height;
    std::vector<int> counts;
};

using ShapeMap = std::unordered_map<int, int>;

// Split text into sections separated by blank lines
std::vector<std::string> split_sections(const std::string& text) {
    std::vector<std::string> sections;
    std::istringstream stream(text);
    std::string line;
    std::string current_section;

    while (std::getline(stream, line)) {
        if (line.empty()) {
            if (!current_section.empty()) {
                sections.push_back(std::move(current_section));
                current_section.clear();
            }
        } else {
            if (!current_section.empty()) {
                current_section += '\n';
            }
            current_section += line;
        }
    }

    if (!current_section.empty()) {
        sections.push_back(std::move(current_section));
    }

    return sections;
}

// Parse a shape definition section
void parse_shape_section(const std::vector<std::string>& lines, ShapeMap& shapes) {
    if (lines.empty()) return;

    const auto& first_line = lines[0];
    const size_t colon_pos = first_line.find(':');
    if (colon_pos == std::string::npos) return;

    const int idx = std::stoi(first_line.substr(0, colon_pos));

    int cell_count = 0;
    for (size_t i = 1; i < lines.size(); ++i) {
        cell_count += std::count(lines[i].begin(), lines[i].end(), '#');
    }

    shapes[idx] = cell_count;
}

// Parse a region definition line
bool parse_region_line(const std::string& line, Region& region) {
    const size_t colon_pos = line.find(':');
    const size_t x_pos = line.find('x');

    if (colon_pos == std::string::npos || x_pos == std::string::npos) {
        return false;
    }

    region.width = std::stoi(line.substr(0, x_pos));
    region.height = std::stoi(line.substr(x_pos + 1, colon_pos - x_pos - 1));

    std::istringstream counts_stream(line.substr(colon_pos + 1));
    region.counts.clear();

    int count;
    while (counts_stream >> count) {
        region.counts.push_back(count);
    }

    return true;
}

// Parse regions section
void parse_regions_section(const std::vector<std::string>& lines, std::vector<Region>& regions) {
    for (const auto& line : lines) {
        Region region;
        if (parse_region_line(line, region)) {
            regions.push_back(std::move(region));
        }
    }
}

// Main parsing function
std::pair<ShapeMap, std::vector<Region>> parse_input(const std::string& text) {
    ShapeMap shapes;
    std::vector<Region> regions;

    const auto sections = split_sections(text);

    for (const auto& section : sections) {
        std::istringstream section_stream(section);
        std::vector<std::string> lines;
        std::string line;

        while (std::getline(section_stream, line)) {
            lines.push_back(line);
        }

        if (lines.empty()) continue;

        const auto& first_line = lines[0];

        // Check if it's a shape definition (has ':' but not 'x')
        if (first_line.find(':') != std::string::npos &&
            first_line.find('x') == std::string::npos) {
            parse_shape_section(lines, shapes);
        } else {
            parse_regions_section(lines, regions);
        }
    }

    return {std::move(shapes), std::move(regions)};
}

// Check if shapes can fit in a region
[[nodiscard]] bool can_fit_region(
    const int width,
    const int height,
    const std::vector<int>& counts,
    const ShapeMap& shape_sizes) {

    int total_cells_needed = 0;

    for (size_t i = 0; i < counts.size(); ++i) {
        if (const auto it = shape_sizes.find(static_cast<int>(i)); it != shape_sizes.end()) {
            total_cells_needed += counts[i] * it->second;
        }
    }

    const int available = width * height;
    return total_cells_needed <= available;
}

[[nodiscard]] int part1(const ShapeMap& shapes, const std::vector<Region>& regions) {
    return std::count_if(regions.begin(), regions.end(),
        [&shapes](const Region& region) {
            return can_fit_region(region.width, region.height, region.counts, shapes);
        });
}

[[nodiscard]] constexpr int part2() {
    // Part 2 is just a button click to finish - no computation needed
    return 0;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error opening input file\n";
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    const std::string text = buffer.str();

    const auto [shapes, regions] = parse_input(text);

    std::cout << "Part 1: " << part1(shapes, regions) << '\n';
    std::cout << "Part 2: " << part2() << '\n';

    return 0;
}
