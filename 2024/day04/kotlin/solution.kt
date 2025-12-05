import java.io.File

fun main() {
    val input = File("../input.txt").readText().trim()
    val grid = input.split("\n")
    val rows = grid.size
    val cols = grid[0].length

    // Part 1: Find all occurrences of "XMAS"
    fun part1(): Int {
        val target = "XMAS"
        var count = 0

        // 8 directions: right, left, down, up, and 4 diagonals
        val directions = listOf(
            Pair(0, 1),   // right
            Pair(0, -1),  // left
            Pair(1, 0),   // down
            Pair(-1, 0),  // up
            Pair(1, 1),   // down-right
            Pair(1, -1),  // down-left
            Pair(-1, 1),  // up-right
            Pair(-1, -1)  // up-left
        )

        for (r in 0 until rows) {
            for (c in 0 until cols) {
                // Try each direction from this position
                for ((dr, dc) in directions) {
                    var found = true
                    for (i in target.indices) {
                        val nr = r + dr * i
                        val nc = c + dc * i
                        if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                            found = false
                            break
                        }
                        if (grid[nr][nc] != target[i]) {
                            found = false
                            break
                        }
                    }
                    if (found) {
                        count++
                    }
                }
            }
        }

        return count
    }

    // Part 2: Find X-MAS patterns (two MAS strings forming an X)
    fun part2(): Int {
        var count = 0

        // Check each possible center point (A must be in the middle)
        for (r in 1 until rows - 1) {
            for (c in 1 until cols - 1) {
                if (grid[r][c] != 'A') {
                    continue
                }

                // Get the four corners
                val topLeft = grid[r - 1][c - 1]
                val topRight = grid[r - 1][c + 1]
                val bottomLeft = grid[r + 1][c - 1]
                val bottomRight = grid[r + 1][c + 1]

                // Check diagonal 1 (top-left to bottom-right): MAS or SAM
                val diag1Ok = (topLeft == 'M' && bottomRight == 'S') ||
                              (topLeft == 'S' && bottomRight == 'M')

                // Check diagonal 2 (top-right to bottom-left): MAS or SAM
                val diag2Ok = (topRight == 'M' && bottomLeft == 'S') ||
                              (topRight == 'S' && bottomLeft == 'M')

                if (diag1Ok && diag2Ok) {
                    count++
                }
            }
        }

        return count
    }

    println("Part 1: ${part1()}")
    println("Part 2: ${part2()}")
}
