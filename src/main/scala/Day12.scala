import scala.io.Source

object Day12:

  def part1(input: List[String]): Int = ???

  def part2(input: List[String]): Int = ???

@main def main12: Unit = 
  val input = Source.fromFile("input/day12.txt").getLines.toList

  println(s"Day12 part1: ${Day12.part1(input)}")
  println(s"Day12 part2: ${Day12.part2(input)}")