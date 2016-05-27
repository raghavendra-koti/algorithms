package self.koti.algorithms

import scala.util.{Failure, Success, Try}

object Sudoku extends App {

  type Grid = List[List[Option[Int]]]

  val rows = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

  /**
   * {(List(1, 2, 3), List(1, 2, 3)) -> 1, (List(1, 2, 3), List(4, 5, 6)) -> 2 etc...)
   * =>  {(1, 1) -> 1, (1, 2) -> 1, (1, 3) -> 1, (2, 1) -> 1, (2, 2) -> 1, (2, 3) -> 1, (3, 1) -> 1, (3, 2) -> 1, (3, 3) -> 1,
   * (1, 4) -> 2, (1, 5) -> 2, (1, 6) -> 2, (2, 4) -> 2, (2, 5) -> 2, (2, 6) -> 2, (3, 4) -> 2, (3, 5) -> 2, (3, 6) -> 2
   * ...}
   */
  val squareMap = (
    for {
      ((x, y), sq) <- (for (x <- rows; y <- rows) yield (x, y)) zip (Stream from 1)
      x_ <- x
      y_ <- y
    }
      yield (x_, y_) -> sq
    ).toMap


  val revSquareMap = Map(
    1 -> Position(0, 0), 2 -> Position(0, 3), 3 -> Position(0, 6),
    4 -> Position(3, 0), 5 -> Position(3, 3), 6 -> Position(3, 6),
    7 -> Position(6, 0), 8 -> Position(6, 3), 9 -> Position(6, 6)
  )

  val one = Some(1)
  val two = Some(2)
  val three = Some(3)
  val four = Some(4)
  val five = Some(5)
  val six = Some(6)
  val seven = Some(7)
  val eight = Some(8)
  val nine = Some(9)
  val masterSet: Set[Option[Int]] = Set(one, two, three, four, five, six, seven, eight, nine)
  val input = List(
    List(None, None, None, two, six, None, seven, None, one),
    List(six, eight, None, None, seven, None, None, nine, None),
    List(one, nine, None, None, None, four, five, None, None),
    List(eight, two, None, one, None, None, None, four, None),
    List(None, None, four, six, None, two, nine, None, None),
    List(None, five, None, None, None, three, None, two, eight),
    List(None, None, nine, three, None, None, None, seven, four),
    List(None, four, None, None, five, None, None, three, six),
    List(seven, None, three, None, one, eight, None, None, None)
  )
  val input1 = List(
    List(four, None, None, two, six, None, seven, None, one),
    List(six, eight, None, None, seven, None, None, nine, None),
    List(one, nine, None, None, None, four, five, None, None),
    List(eight, two, None, one, None, None, None, four, None),
    List(None, None, four, six, None, two, nine, None, None),
    List(None, five, None, None, None, three, None, two, eight),
    List(None, None, nine, three, None, None, None, seven, four),
    List(None, four, None, None, five, None, None, three, six),
    List(seven, None, three, None, one, eight, None, None, two)
  )
  val input2 = List(
    List(None, None, None, two, None, None, None, six, three),
    List(three, None, None, None, None, five, four, None, one),
    List(None, None, one, None, None, three, nine, eight, None),
    List(None, None, None, None, None, None, None, nine, None),
    List(None, None, None, five, three, eight, None, None, None),
    List(None, three, None, None, None, None, None, None, None),
    List(None, two, six, three, None, None, five, None, None),
    List(five, None, three, seven, None, None, None, None, eight),
    List(four, seven, None, None, None, one, None, None, None)
  )
  val hardest = List(
    List(eight, None, None, None, None, None, None, None, None),
    List(None, None, three, six, None, None, None, None, None),
    List(None, seven, None, None, nine, None, two, None, None),
    List(None, five, None, None, None, seven, None, None, None),
    List(None, None, None, None, four, five, seven, None, None),
    List(None, None, None, one, None, None, None, three, None),
    List(None, None, one, None, None, None, None, six, eight),
    List(None, None, eight, five, None, None, None, one, None),
    List(None, nine, None, None, None, None, four, None, None)
  )
  val output = List(
    List(four, three, five, two, six, nine, seven, eight, one),
    List(six, eight, two, five, seven, one, four, nine, three),
    List(one, nine, seven, eight, three, four, five, six, two),
    List(eight, two, six, one, nine, five, three, four, seven),
    List(three, seven, four, six, eight, two, nine, one, five),
    List(nine, five, one, seven, four, three, six, two, eight),
    List(five, one, nine, three, two, six, eight, seven, four),
    List(two, four, eight, nine, five, seven, one, three, six),
    List(seven, six, three, four, one, eight, two, five, nine)
  )
  val malformedGrid =
    List(
      List(four, three, four, two, six, nine, seven, eight, one),
      List(six, eight, two, five, seven, one, four, nine, three),
      List(one, nine, seven, eight, three, four, five, six, two),
      List(eight, two, six, one, nine, five, three, four, seven),
      List(three, seven, four, six, eight, two, nine, one, five),
      List(nine, five, one, seven, four, three, six, two, eight),
      List(five, one, nine, three, two, six, eight, seven, four),
      List(two, four, eight, nine, five, seven, one, three, six),
      List(seven, six, three, four, one, eight, two, five, nine)
    )
  val result = getSquare(input, 1)

  assert(input == input, "Not true")
  assert(output == output, "output not equal to output")
  val expectedResult = List[Option[Int]](None, None, None, six, eight, None, one, nine, None)

  def printGrid(grid: Grid) = grid map { row => row map { column => " " + print(column) } }

  assert(result == expectedResult)
  assert(getRow(input, 1) == List(None, None, None, two, six, None, seven, None, one))
  assert(getColumn(input, 1) == List(None, six, one, eight, None, None, None, None, seven))
  assert(isValid(List[Option[Int]](None, None, None, None, None, None, None, None, None)))
  assert(isValid(List[Option[Int]](one, two, three, four, five, six, seven, eight, nine)))
  assert(!isValid(List[Option[Int]](one, two, None, four, five, None, seven, seven, nine)))
  assert(isGridValid(input))
  assert(isGridValid(output))
  assert(!isGridValid(malformedGrid))
  assert(getSquareNumber(Position(1, 1)) == 1)
  assert(getSquareNumber(Position(9, 9)) == 9)
  assert(getAlternatives(Position(1, 1), input).toSet == Set(three, four, five))
  println(fillValue(input, Position(1, 1), Some(4)))
  assert(fillValue(fillValue(input, Position(1, 1), Some(4)), Position(9, 9), Some(2)) == input1)
  assert(solve(input) == output)
  time {
    printGrid(solve(input2))
  }
  time {
    printGrid(solve(hardest))
  }

  // println(isResolved(input))
  //assert(solve(input) == output, "NOT SOLVED")

  def solve(grid: Grid): Grid = solve(grid, getPossibleSols(grid)).get

  /**
   * Solves the grid given Grid and the choices
   *
   * 1. Check if the grid is already solved, if yes, then return success(grid)
   * 2. Otherwise if the given grid is invalid:
   * 2.1 Return Failure(grid)
   * 3. Otherwise (Grid is still unsolved)
   * 3.1 sort the choices for positions based on length of the list of alternatives (ascending)
   * 3.2 get the first possible fill and get new choices and solve the new grid after filling the value recursively
   * @param grid
   * @param choices
   * @return
   */
  def solve(grid: Grid, choices: Map[Position, List[Option[Int]]]): Try[Grid] = {
    if (isResolved(grid)) {
      Success(grid)
    }
    else if (!isGridValid(grid)) {
      Failure(new Exception("No use going down this path!"))
    }
    else {
      val sChoices = choices.toSeq.sortWith {
        case (element1, element2) =>
          element1._2.length < element2._2.length
      }
      val (position, list) = sChoices.head
      val initial: Try[Grid] = Failure(new Exception(" "))
      list.foldLeft(initial) {
        case (maybeGrid: Try[Grid], value: Option[Int]) =>
          maybeGrid match {
            case Success(_) => maybeGrid
            case Failure(_) =>
              val newPossibleChoices = modifySearchSpace(choices -- Set(position), position, value)
              solve(fillValue(grid, position, value), newPossibleChoices)
          }
      }
    }
  }

  /**
   * Modifies possibleChoices by removing a given value from position
   * @param possibleChoices
   * @param inPosition
   * @param value
   * @return
   */
  def modifySearchSpace(possibleChoices: Map[Position, List[Option[Int]]],
                        inPosition: Position,
                        value: Option[Int]): Map[Position, List[Option[Int]]] = {
    for ((position, list) <- possibleChoices)
      yield {
        if (inPosition.x == position.x
          || inPosition.y == position.y
          || getSquareNumber(position) == getSquareNumber(inPosition))
          position -> list.filterNot(_ == value)
        else
          position -> list
      }
  }

  /**
   * Checks if we have the solution ready. Grid should be non-empty and fully filled (defined)
   * @param grid
   * @return
   */
  def isResolved(grid: Grid) = (for (row <- grid; col <- row) yield col) forall (_.isDefined)

  /**
   * Finds possible values that can exist for all empty cells in the grid
   * @param grid
   * @return
   */
  def getPossibleSols(grid: Grid): Map[Position, List[Option[Int]]] = {
    (for ((row, xIndex) <- grid.zip(Stream from 1);
          (value, yIndex) <- row.zip(Stream from 1) if value.isEmpty)
      yield Position(xIndex, yIndex) -> getAlternatives(Position(xIndex, yIndex), grid)).toMap
  }

  /**
   * Given a cell and grid, gives valid values that can exist in the cell
   * @param position
   * @param grid
   * @return
   */
  def getAlternatives(position: Position, grid: Grid): List[Option[Int]] = {
    val values = (getRow(grid, position.x) ++ getColumn(grid, position.y) ++ getSquare(grid, getSquareNumber(position))).toSet
    (masterSet -- values).toList
  }

  /**
   * Gets the square id given position
   * @param position
   * @return
   */
  def getSquareNumber(position: Position): Int = squareMap((position.x, position.y))

  /**
   * Fill empty cell with the value provided
   * @param grid
   * @param position
   * @param value
   * @return
   */
  def fillValue(grid: Grid, position: Position, value: Option[Int]): Grid = {
    require(position.x >= 1 && position.x <= 9, "x coordinate out of bounds")
    require(position.y >= 1 && position.y <= 9, "y coordinate out of bounds")
    require(grid(position.x - 1)(position.y - 1).isEmpty, "Can't fill in non-empty box")

    for ((row, xIndex) <- grid.zip(Stream from 1))
      yield if (xIndex == position.x) row.updated(position.y - 1, value) else row
  }

  /**
   * Figures if the whole grid is valid by checking if row is valid, column is valid and square is valid
   * @param grid
   * @return Boolean
   */
  def isGridValid(grid: Grid): Boolean =
    (for (i <- Range(1, 9)) yield
    isValid(getRow(grid, i)) && isValid(getColumn(grid, i)) && isValid(getSquare(grid, i))
      ).forall(_ == true)

  /**
   * Finds if the list does not contain duplicate values. Ignores undefined (unfilled) values
   * @param input
   * @return
   */
  def isValid(input: List[Option[Int]]): Boolean = {
    val defined = input.filter(_.isDefined)
    defined.toSet.size == defined.size
  }

  /**
   * Gets a chunk out of the grid for the square
   * Get the anchor cell and then slices the next three cells y-side for each of the next three x-sides
   * @param grid
   * @param which
   * @return
   */
  def getSquare(grid: Grid, which: Int): List[Option[Int]] = {
    require(1 <= which && which <= 9, "which is out of range [1, 9]")
    val (x_, y_) = (revSquareMap(which).x, revSquareMap(which).y)

    (0 to 2).foldLeft(List.empty[Option[Int]]) {
      case (list, index) => list ++ grid(x_ + index).slice(y_, y_ + 3)
    }
  }

  /**
   * Slices the column from the grid using each of the lists (rows)
   * @param grid
   * @param which
   * @return
   */
  def getColumn(grid: Grid, which: Int): List[Option[Int]] = {
    require(1 <= which && which <= 9, "which is out of range [1, 9]")
    for (row <- grid) yield row(which - 1)
  }

  /**
   * Just returns the nth list as these represent the rows
   * @param grid
   * @param which
   * @return
   */
  def getRow(grid: Grid, which: Int): List[Option[Int]] = {
    require(1 <= which && which <= 9, s"$which is out of range [1, 9]")
    grid(which - 1)
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0).toDouble / 1000000.toDouble + "ms")
    result
  }

  case class Position(x: Int, y: Int)

}

