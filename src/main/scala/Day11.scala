import scala.io.Source
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

object Day11:

  def adjacent(x: Int, y: Int): List[(Int, Int)] =
    List(
      (x, y + 1), 
      (x, y - 1), 
      (x + 1, y), 
      (x - 1, y), 
      (x + 1, y + 1), 
      (x - 1, y + 1), 
      (x + 1, y - 1), 
      (x - 1, y - 1))

  def step(input: List[List[Int]]): (Int, List[List[Int]]) = 
    val increment = input.map(_.map(_ + 1))

    val all = for {
      x <- 0 until input(0).size
      y <- 0 until input.size
    } yield (x, y)

    val energy = all.filter { (x, y) => increment(y)(x) > 9 }
    val adjacentIncrement = energy.flatMap(adjacent).filter {
        (x, y) => x > -1 && y > -1 && x < input(0).size && y < input.size
    }
    val (count, feedback) = adjacentIncrement.foldLeft((energy.size, increment)) {
      case ((c, state), (x, y)) => (if (state(y)(x) > 9) c + 1 else c, state.updated(y, state(y).updated(x, state(y)(x) + 1)))
    }

    (count, feedback.map(_.map(x => if (x > 9) 0 else x)))

  @tailrec
  def play(n: Int, count: Int, input: List[List[Int]]): (Int, List[List[Int]]) =
    if (n > 0)
      val (x, next) = step(input)
      play(n - 1, count + x, next)
    else
      (count, input)

  def part1(input: List[String]): Int =
    val parsed = input.map(_.map(_.toString.toInt).toList)
    play(100, 0, parsed)._1

  def part2(input: List[String]): Int = ???

@main def main11: Unit = 
  val input = Source.fromFile("input/day11.txt").getLines.toList

  println(s"Day11 part1: ${Day11.part1(input)}")
  println(s"Day11 part2: ${Day11.part2(input)}")