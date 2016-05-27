package self.koti.algorithms


case class Position(x: Int, y: Int)

object NQueens extends App {

  /**
   * This version of the NQueens solve uses flatMap and map.
   * Overall algorithm:
   * 1. For solving the problem for column k
   * 1.1 Solve the problem for column k-1
   * 1.2 For each such solution:
   * 1.2.1 Place k in all possible rows
   * 1.2.2 Weed out unsafe solutions
   * @param n
   * @return
   */
  def solve(n: Int): List[List[Position]] = {
    def solve(positions: List[List[Position]], k: Int): List[List[Position]] = {
      if (k == 0) {
        List(List.empty[Position])
      }
      else {
        solve(positions, k - 1).flatMap {
          restOfQueens => (1 to n).map {
            row => {
              Position(row, k) :: restOfQueens
            }
          }
        }.filter {
          x => isSafe(x)
        }
      }
    }
    solve(List.empty[List[Position]], n)
  }

  /**
   * This version of the NQueens solve uses flatMap and map.
   * Overall algorithm:
   * 1. For solving the problem for column k
   * 1.1 Solve the problem for column k-1
   * 1.2 For each such solution:
   * 1.2.1 Place k in all possible rows
   * 1.2.2 Weed out unsafe solutions
   * @param n
   * @return
   */
  def solveFor(n: Int) = {
    def solveFor(positions: List[List[Position]], k: Int): List[List[Position]] = {
      if (k == 0) {
        List(List.empty[Position])
      }
      else {
        for {
          restOfQueens <- solveFor(positions, k - 1)
          row <- 1 to n
          s = Position(row, k) :: restOfQueens
          if isSafe(s)
        } yield s
      }
    }
    solveFor(List.empty[List[Position]], n)
  }

  /**
   * Returns if last position is safe with respect to other positions.
   * @param positions
   * @return
   */
  def isSafe(positions: List[Position]): Boolean = {
    val target = positions.head
    val others = positions.filterNot(_ == target)
    others.forall(
      position =>
        !(position.x == target.x) &&
          !(Math.abs(position.x - target.x) == Math.abs(position.y - target.y))
    )
  }

  assert(isSafe(List(Position(1, 1), Position(3, 2))))
  assert(isSafe(List(Position(1, 1), Position(3, 2), Position(5, 3))))
  assert(isSafe(List(Position(1, 1), Position(3, 2), Position(5, 3), Position(2, 4))))
  assert(!isSafe(List(Position(1, 1), Position(2, 2))))
  assert(solve(5).size == 10)
  assert(solveFor(5).size == 10)
  assert(solve(8).size == 92)
  println(solveFor(14).size)
}


