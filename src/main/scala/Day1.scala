import scala.io.Source

object Day1:

  def part1(input: List[Int]): Int =
    val tuples = input.dropRight(1) zip input.drop(1)
    tuples.count({ case (a, b) => b > a })

  def part2(input: List[Int]): Int =
    val sums = input.sliding(3).map(_.sum).toList
    part1(sums)

@main def main: Unit = 
  val numbers = Source.fromFile("input/day1.txt").getLines.map(_.toInt).toList

  println(Day1.part1(numbers))
  println(Day1.part2(numbers))