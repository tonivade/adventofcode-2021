import scala.io.Source

object Day13:

  def part1(input: List[String]): Int = ???

  def part2(input: List[String]): Int = ???

@main def main13: Unit = 
  val input = Source.fromFile("input/day13.txt").getLines.toList

  println(s"Day13 part1: ${Day13.part1(input)}")
  println(s"Day13 part2: ${Day13.part2(input)}")