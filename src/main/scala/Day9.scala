import scala.io.Source

object Day9:

  case class Position(x: Int, y: Int):
    def adjacent: List[Position] = 
      List(
        Position(x - 1, y),
        Position(x + 1, y),
        Position(x, y - 1),
        Position(x, y + 1)
      )

  def findLowerPoints(board: Map[Position, Int]): List[Int] = 
    board.map { (position, value) => 
      if (position.adjacent.map(board.get).flatMap(_.toList).forall(_ > value))
        Some(value)
      else
        None
    }.flatMap(_.toList).toList

  def findBasins(board: Map[Position, Int]): List[List[(Position, Int)]] = 
    board.map { (position, value) => 
      if (position.adjacent.map(board.get).flatMap(_.toList).forall(_ > value))
        (position, value) :: search(board, position, value)
      else
        List.empty
    }.toList.filterNot(_.isEmpty)

  def search(board: Map[Position, Int], position: Position, value: Int): List[(Position, Int)] =
    val adjacents = position.adjacent.map(p => board.get(p).map((p, _))).flatMap(_.toList)
    val r = adjacents.filter((_, x) => x > value).flatMap { 
      case (p, 9) => List.empty
      case (p, x) => (p, x) :: search(board, p, x)
    }
    r.toSet.toList

  def parse(input: List[String]): Map[Position, Int] =
    val width = input(0).size
    val height = input.size

    val all = for {
      i <- 0 until height
      j <- 0 until width
    } yield (Position(i, j), input(i)(j).toString.toInt)

    all.toMap

  def part1(input: List[String]): Int = 
    val map = parse(input)

    val lowerPoints = findLowerPoints(map)

    lowerPoints.map(_ + 1).sum

  def part2(input: List[String]): Int =
    val map = parse(input)

    val basins = findBasins(map)

    basins.map(_.size).sorted.reverse.take(3).foldLeft(1)(_ * _)

@main def main9: Unit = 
  val input = Source.fromFile("input/day9.txt").getLines.toList

  println(s"Day9 part1: ${Day9.part1(input)}")
  println(s"Day9 part2: ${Day9.part2(input)}")