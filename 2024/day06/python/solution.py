from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    """Simulate guard patrol and count distinct positions visited."""
    # Parse the grid
    grid = lines

    # Find the guard's starting position and direction
    guard_pos = None
    guard_dir = None
    direction_chars = {'^': (0, -1), 'v': (0, 1), '<': (-1, 0), '>': (1, 0)}

    for y, line in enumerate(grid):
        for x, char in enumerate(line):
            if char in direction_chars:
                guard_pos = (x, y)
                guard_dir = direction_chars[char]
                break
        if guard_pos:
            break

    # Helper functions
    def turn_right(direction):
        """Turn right 90 degrees."""
        dx, dy = direction
        # (0, -1) -> (1, 0) -> (0, 1) -> (-1, 0) -> (0, -1)
        return (-dy, dx)

    def is_obstacle(x, y):
        """Check if position (x, y) is an obstacle."""
        if y < 0 or y >= len(grid):
            return False
        if x < 0 or x >= len(grid[y]):
            return False
        return grid[y][x] == '#'

    def is_in_bounds(x, y):
        """Check if position (x, y) is within the grid bounds."""
        return 0 <= y < len(grid) and 0 <= x < len(grid[y])

    # Simulate the guard's patrol
    visited = set()
    visited.add(guard_pos)

    x, y = guard_pos
    dx, dy = guard_dir

    while True:
        # Calculate next position
        next_x = x + dx
        next_y = y + dy

        # Check if guard would leave the map
        if not is_in_bounds(next_x, next_y):
            break

        # Check if there's an obstacle ahead
        if is_obstacle(next_x, next_y):
            # Turn right
            dx, dy = turn_right((dx, dy))
        else:
            # Move forward
            x = next_x
            y = next_y
            visited.add((x, y))

    return len(visited)


def part2():
    """Count positions where adding an obstruction creates a loop."""
    # Parse the grid
    grid = [list(line) for line in lines]

    # Find the guard's starting position and direction
    guard_pos = None
    guard_dir = None
    direction_chars = {'^': (0, -1), 'v': (0, 1), '<': (-1, 0), '>': (1, 0)}

    for y, line in enumerate(grid):
        for x, char in enumerate(line):
            if char in direction_chars:
                guard_pos = (x, y)
                guard_dir = direction_chars[char]
                break
        if guard_pos:
            break

    # Helper functions
    def turn_right(direction):
        """Turn right 90 degrees."""
        dx, dy = direction
        return (-dy, dx)

    def is_obstacle(grid_state, x, y):
        """Check if position (x, y) is an obstacle."""
        if y < 0 or y >= len(grid_state):
            return False
        if x < 0 or x >= len(grid_state[y]):
            return False
        return grid_state[y][x] == '#'

    def is_in_bounds(x, y):
        """Check if position (x, y) is within the grid bounds."""
        return 0 <= y < len(grid) and 0 <= x < len(grid[y])

    def simulate_patrol(grid_state):
        """Simulate patrol and return visited positions."""
        visited = set()
        visited.add(guard_pos)

        x, y = guard_pos
        dx, dy = guard_dir

        while True:
            next_x = x + dx
            next_y = y + dy

            if not is_in_bounds(next_x, next_y):
                break

            if is_obstacle(grid_state, next_x, next_y):
                dx, dy = turn_right((dx, dy))
            else:
                x = next_x
                y = next_y
                visited.add((x, y))

        return visited

    def detect_loop(grid_state):
        """Check if the guard gets stuck in a loop."""
        states = set()
        x, y = guard_pos
        dx, dy = guard_dir

        while True:
            state = (x, y, dx, dy)
            if state in states:
                return True
            states.add(state)

            next_x = x + dx
            next_y = y + dy

            if not is_in_bounds(next_x, next_y):
                return False

            if is_obstacle(grid_state, next_x, next_y):
                dx, dy = turn_right((dx, dy))
            else:
                x = next_x
                y = next_y

    # First, get all positions the guard visits in normal patrol
    visited = simulate_patrol(grid)

    # Count positions where adding an obstruction creates a loop
    loop_count = 0

    for pos in visited:
        if pos == guard_pos:
            continue  # Can't place obstruction at starting position

        # Place obstruction
        x, y = pos
        original = grid[y][x]
        grid[y][x] = '#'

        # Check if this creates a loop
        if detect_loop(grid):
            loop_count += 1

        # Remove obstruction
        grid[y][x] = original

    return loop_count


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
