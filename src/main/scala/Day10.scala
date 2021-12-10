import scala.io.Source

object Day10:


  def part1(input: List[String]): Int = ???

  def part2(input: List[String]): Int = ???

@main def main10: Unit = 
  val input = Source.fromFile("input/day10.txt").getLines.toList

  println(s"Day10 part1: ${Day10.part1(input)}")
  println(s"Day10 part2: ${Day10.part2(input)}")