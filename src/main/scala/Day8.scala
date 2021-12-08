import scala.io.Source

object Day8:

  def part1(input: String): Int = ???

  def part2(input: String): Int = ???

@main def main8: Unit = 
  val input = Source.fromFile("input/day8.txt").getLines.next

  println(s"Day8 part1: ${Day8.part1(input)}")
  println(s"Day8 part2: ${Day8.part2(input)}")