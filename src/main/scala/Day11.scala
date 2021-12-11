import scala.io.Source
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.annotation.tailrec

object Day11:

  case class Board(input: List[List[Int]]):

    val height = input.size
    val width = input(0).size

    def all = for {
      x <- 0 until width
      y <- 0 until height
    } yield (x, y)

    def adjacent(x: Int, y: Int): List[(Int, Int)] =
      List(
        (x, y + 1), 
        (x, y - 1), 
        (x + 1, y), 
        (x - 1, y), 
        (x + 1, y + 1), 
        (x - 1, y + 1), 
        (x + 1, y - 1), 
        (x - 1, y - 1)).filter {
          (x, y) => x > -1 && y > -1 && x < input(0).size && y < input.size
        }
    
    def increment = Board(input.map(_.map(_ + 1)))

    def get(x: Int, y: Int) = input(y)(x)

    def update(x: Int, y: Int): Board = 
      Board(input.updated(y, input(y).updated(x, get(x, y) + 1)))

    def clear = Board(input.map(_.map(x => if (x > 9) 0 else x)))

    def allFlash = input.flatMap(identity).sum == 0

    def step: (Int, Board) = 
      val (state, output) = all.foldLeft((Set.empty[(Int, Int)], increment)) {
        case ((s, i), (x, y)) => flash(x, y)(s, i)
      }

      (state.size, output.clear)
  
    def flash(x: Int, y: Int)(state: Set[(Int, Int)], input: Board): (Set[(Int, Int)], Board) =
      if (!state.contains(x, y) && input.get(x, y) > 9)
        val adj = input.adjacent(x, y)
        adj.foldLeft((state + ((x, y)), input)) {
          case ((s, i), (x1, y1)) => flash(x1, y1)(s, i.update(x1, y1))
        }
      else 
        (state, input)

  @tailrec
  def play(n: Int, count: Int, input: Board): (Int, Board) =
    if (n > 0)
      val (x, next) = input.step
      play(n - 1, count + x, next)
    else
      (count, input)

  @tailrec
  def sync(n: Int, input: Board): Int =
    if (!input.allFlash)
      val (_, next) = input.step
      sync(n + 1, next)
    else
      n

  def part1(input: List[String]): Int =
    val parsed = input.map(_.map(_.toString.toInt).toList)
    play(100, 0, Board(parsed))._1

  def part2(input: List[String]): Int =
    val parsed = input.map(_.map(_.toString.toInt).toList)
    sync(0, Board(parsed))

@main def main11: Unit = 
  val input = Source.fromFile("input/day11.txt").getLines.toList

  println(s"Day11 part1: ${Day11.part1(input)}")
  println(s"Day11 part2: ${Day11.part2(input)}")