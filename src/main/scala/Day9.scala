import scala.io.Source
import Day4.Position

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

  def part1(input: List[String]): Int = 
    val width = input(0).size
    val height = input.size

    val all = for {
      i <- 0 until height
      j <- 0 until width
    } yield (Position(i, j), input(i)(j).toString.toInt)

    val lowerPoints = findLowerPoints(all.toMap)

    lowerPoints.map(_ + 1).sum

  def part2(input: List[String]): Int = ???

@main def main9: Unit = 
  val input = Source.fromFile("input/day9.txt").getLines.toList

  println(s"Day9 part1: ${Day9.part1(input)}")
  println(s"Day9 part2: ${Day9.part2(input)}")