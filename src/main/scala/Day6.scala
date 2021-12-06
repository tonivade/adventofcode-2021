import scala.io.Source
import scala.annotation.tailrec

object Day6:

  @tailrec
  def step(days: Int, fishes: Map[Int, Long]): Map[Int, Long] = 
    if (days == 0)
      fishes
    else 
      val fishes0 = fishes.getOrElse(0, 0L)
      step(days - 1, Map(
        0 -> fishes.getOrElse(1, 0L), 
        1 -> fishes.getOrElse(2, 0L), 
        2 -> fishes.getOrElse(3, 0L), 
        3 -> fishes.getOrElse(4, 0L), 
        4 -> fishes.getOrElse(5, 0L), 
        5 -> fishes.getOrElse(6, 0L), 
        6 -> (fishes.getOrElse(7, 0L) + fishes0),
        7 -> fishes.getOrElse(8, 0L),
        8 -> fishes0))

  def parse(input: String): Map[Int, Long] =
    input.split(",").map(_.toInt).groupBy(identity).map((a, b) => (a, b.size.toLong)).toMap

  def part1(input: String): Long = 
    step(80, parse(input)).values.sum

  def part2(input: String): Long = 
    step(256, parse(input)).values.sum

@main def main6: Unit = 
  val input = Source.fromFile("input/day6.txt").getLines.next

  println(s"Day6 part1: ${Day6.part1(input)}")
  println(s"Day6 part2: ${Day6.part2(input)}")