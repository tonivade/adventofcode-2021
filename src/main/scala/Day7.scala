import scala.io.Source
import scala.annotation.tailrec

object Day7:

  def distanceTo(position: Int)(crab: Int) =
    (crab - position).abs

  def part1(input: String): Int = 
    val crabs = input.split(",").map(_.toInt)

    val costs = (1 to crabs.size).map { i =>
      crabs.map(distanceTo(i)).sum
    }

    costs.min


  def part2(input: String): Int = ???

@main def main7: Unit = 
  val input = Source.fromFile("input/day7.txt").getLines.next

  println(s"Day7 part1: ${Day7.part1(input)}")
  println(s"Day7 part2: ${Day7.part2(input)}")