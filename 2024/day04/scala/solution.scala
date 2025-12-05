import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    // Read input
    val inputPath = "../input.txt"
    val grid = Source.fromFile(inputPath).getLines().toVector
    val rows = grid.length
    val cols = grid(0).length

    // 8 directions: right, left, down, up, and 4 diagonals
    val directions = Vector(
      (0, 1),    // right
      (0, -1),   // left
      (1, 0),    // down
      (-1, 0),   // up
      (1, 1),    // down-right
      (1, -1),   // down-left
      (-1, 1),   // up-right
      (-1, -1)   // up-left
    )

    def part1(): Int = {
      val target = "XMAS"
      var count = 0

      for {
        r <- 0 until rows
        c <- 0 until cols
        (dr, dc) <- directions
      } {
        // Check if XMAS fits in this direction
        var found = true
        for (i <- 0 until target.length if found) {
          val nr = r + dr * i
          val nc = c + dc * i
          if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
            found = false
          } else if (grid(nr)(nc) != target(i)) {
            found = false
          }
        }
        if (found) count += 1
      }

      count
    }

    def part2(): Int = {
      // Find X-MAS patterns: two MAS strings forming an X with A in the center
      // Each diagonal can be MAS or SAM
      var count = 0

      // Check each possible center point (A must be in the middle)
      for {
        r <- 1 until (rows - 1)
        c <- 1 until (cols - 1)
        if grid(r)(c) == 'A'
      } {
        // Get the four corners
        val topLeft = grid(r - 1)(c - 1)
        val topRight = grid(r - 1)(c + 1)
        val bottomLeft = grid(r + 1)(c - 1)
        val bottomRight = grid(r + 1)(c + 1)

        // Check diagonal 1 (top-left to bottom-right): MAS or SAM
        val diag1Ok = (topLeft == 'M' && bottomRight == 'S') || (topLeft == 'S' && bottomRight == 'M')

        // Check diagonal 2 (top-right to bottom-left): MAS or SAM
        val diag2Ok = (topRight == 'M' && bottomLeft == 'S') || (topRight == 'S' && bottomLeft == 'M')

        if (diag1Ok && diag2Ok) count += 1
      }

      count
    }

    println(s"Part 1: ${part1()}")
    println(s"Part 2: ${part2()}")
  }
}
