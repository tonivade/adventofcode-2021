import scala.io.Source
import scala.annotation.tailrec

object Day7:

  def distanceTo(position: Int)(crab: Int): Int =
    (crab - position).abs

  def distance2To(position: Int)(crab: Int): Int =
    distanceTo(position)(crab) match {
      case 0 => 0
      case 1 => 1
      case n => (1 to n).sum
    }

  def parse(input: String): Seq[Int] =
    input.split(",").map(_.toInt)

  def part1(input: String): Int = 
    val crabs = parse(input)

    val costs = (0 to crabs.size).map { i =>
      crabs.map(distanceTo(i)).sum
    }

    costs.min

  def part2(input: String): Int = 
    val crabs = parse(input)

    val costs = (0 to crabs.size).map { i =>
      crabs.map(distance2To(i)).sum
    }

    costs.min

@main def main7: Unit = 
  val input = Source.fromFile("input/day7.txt").getLines.next

  println(s"Day7 part1: ${Day7.part1(input)}")
  println(s"Day7 part2: ${Day7.part2(input)}")