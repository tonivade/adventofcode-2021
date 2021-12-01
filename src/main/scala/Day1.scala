import scala.io.Source

object Day1:

  def solve(input: List[Int]): Int =
    val tuples = input.dropRight(1) zip input.drop(1)
    tuples.count({ case (a, b) => b > a })

@main def main: Unit = 
  val numbers = Source.fromFile("input/day1.txt").getLines.map(_.toInt).toList

  println(Day1.solve(numbers))