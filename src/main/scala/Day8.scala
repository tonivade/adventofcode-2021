import scala.io.Source

object Day8:

  def part1(input: List[String]): Int =
    val uniqueDigits = Set(2, 3, 4, 7)
    val all = input.flatMap(_.split("\\|")(1).trim.split(" "))
    all.filter(x => uniqueDigits.contains(x.size)).size

  def part2(input: List[String]): Int = ???

@main def main8: Unit = 
  val input = Source.fromFile("input/day8.txt").getLines.toList

  println(s"Day8 part1: ${Day8.part1(input)}")
  println(s"Day8 part2: ${Day8.part2(input)}")