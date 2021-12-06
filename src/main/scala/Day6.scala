import scala.io.Source
import scala.annotation.tailrec

object Day6:

  case class Fish(eta: BigInt)

  def step(days: Int, fishes: List[Fish]): List[Fish] = 
    if (days == 0)
      fishes
    else 
      step(days - 1, fishes.flatMap {
        case Fish(eta) if (eta == 0) => List(Fish(6), Fish(8))
        case Fish(eta) => List(Fish(eta - 1))
      })

  def parse(input: String): List[Fish] =
    input.split(",").map(_.toInt).map(Fish(_)).toList

  def part1(input: String): Int = 
    step(80, parse(input)).size

  def part2(input: String): Int = 
    step(256, parse(input)).size

@main def main6: Unit = 
  val input = Source.fromFile("input/day6.txt").getLines.next

  println(s"Day6 part1: ${Day6.part1(input)}")
  println(s"Day6 part2: ${Day6.part2(input)}")