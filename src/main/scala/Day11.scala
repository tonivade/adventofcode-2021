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

    val after = all.foldLeft((Set.empty[(Int, Int)], increment)) {
      case ((s, i), (x, y)) => flash(x, y)(s, i)
    }

    (after._1.size, after._2.map(_.map(x => if (x > 9) 0 else x)))
  
  def flash(x: Int, y: Int)(state: Set[(Int, Int)], input: List[List[Int]]): (Set[(Int, Int)], List[List[Int]]) =
    if (!state.contains(x, y) && input(y)(x) > 9)
      val adj = adjacent(x, y).filter {
        (x, y) => x > -1 && y > -1 && x < input(0).size && y < input.size
      }
      adj.foldLeft((state + ((x, y)), input)) {
        case ((s, i), (x1, y1)) => flash(x1, y1)(s, i.updated(y1, i(y1).updated(x1, i(y1)(x1) + 1)))
      }
    else 
      (state, input)

  @tailrec
  def play(n: Int, count: Int, input: List[List[Int]]): (Int, List[List[Int]]) =
    if (n > 0)
      val (x, next) = step(input)
      play(n - 1, count + x, next)
    else
      (count, input)

  @tailrec
  def sync(n: Int, input: List[List[Int]]): Int =
    if (input.flatMap(identity).sum != 0)
      val (_, next) = step(input)
      sync(n + 1, next)
    else
      n

  def part1(input: List[String]): Int =
    val parsed = input.map(_.map(_.toString.toInt).toList)
    play(100, 0, parsed)._1

  def part2(input: List[String]): Int =
    val parsed = input.map(_.map(_.toString.toInt).toList)
    sync(0, parsed)

@main def main11: Unit = 
  val input = Source.fromFile("input/day11.txt").getLines.toList

  println(s"Day11 part1: ${Day11.part1(input)}")
  println(s"Day11 part2: ${Day11.part2(input)}")