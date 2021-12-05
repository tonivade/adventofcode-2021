import scala.io.Source
import scala.annotation.tailrec

object Day5:

  case class Position(x: Int, y: Int)

  case class Line(start: Position, end: Position):

    def horizontal: Boolean = (start, end) match {
      case (Position(x1, y1), Position(x2, y2)) if (x1 == x2) => true
      case _ => false
    }

    def vertical: Boolean = (start, end) match {
      case (Position(x1, y1), Position(x2, y2)) if (y1 == y2) => true
      case _ => false
    }

    def diagonal: Boolean = !horizontal && !vertical

    def draw: List[Position] = 
      if (horizontal)
       (Math.min(start.y, end.y) to Math.max(start.y, end.y)).map { y =>
          Position(start.x, y)
        }.toList
      else if (vertical)
        (Math.min(start.x, end.x) to Math.max(start.x, end.x)).map { x =>
          Position(x, end.y)
        }.toList
      else {
        val x = start.x to end.x by (if (start.x < end.x) 1 else -1)
        val y = start.y to end.y by (if (start.y < end.y) 1 else -1)
        x.zip(y).map(Position(_, _)).toList
      }

  def parse(input: String): Array[Line] = 
    input.split("\n").map { line =>
      val splitted = line.split(" ")
      val left = splitted(0).split(",")
      val right = splitted(2).split(",")
      Line(Position(left(0).toInt, left(1).toInt), Position(right(0).toInt, right(1).toInt))
    }

  def part1(input: String): Int = 
    val parsed = parse(input)
    val all = parsed.filterNot(_.diagonal).flatMap(_.draw)
    val grouped = all.groupBy(identity).mapValues(_.size).filter { case (_, x) => x > 1 }
    grouped.size

  def part2(input: String): Int =
    val parsed = parse(input)
    val all = parsed.flatMap(_.draw)
    val grouped = all.groupBy(identity).mapValues(_.size).filter { case (_, x) => x > 1 }
    grouped.size

@main def main5: Unit = 
  val input = Source.fromFile("input/day5.txt").mkString

  println(s"Day5 part1: ${Day5.part1(input)}")
  println(s"Day5 part2: ${Day5.part2(input)}")