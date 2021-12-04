import scala.io.Source
import scala.annotation.tailrec

object Day4:

  case class Position(x: Int, y: Int)

  case class Board(matrix: Map[Position, Int], drawn: List[Int]):
    def lookup(num: Int): Board = 
      if (matrix.values.toSet.contains(num))
        Board(matrix, drawn :+ num)
      else
        this

    def win: Boolean = 
      val positions = matrix.filter {
        case (position, num) => drawn.contains(num)
      }.keySet
      
      val row = (0 to 5).find { x =>
        positions.count(_.x == x) == 5
      }
        
      val col = (0 to 5).find { y =>
        positions.count(_.y == y) == 5
      }

      row.isDefined || col.isDefined
    
    def score: Int = 
      matrix.values.filter(n => !drawn.contains(n)).sum

  case class Game(numbers: Seq[Int], boards: Seq[Board]):
    def take: (Game, Int) = 
      (Game(numbers.tail, boards.map(_.lookup(numbers.head))), numbers.head)
    def winners: Seq[Board] = boards.filter(_.win)
    def removeAll(toRemove: Seq[Board]): Game = Game(numbers, boards.filterNot(toRemove.contains))

  def parse(input: String): Game =
    val splited = input.split("\n\n")
    val numbers = splited.head.split(",").map(_.toInt)

    val boards = splited.tail.map(_.split("\n")).zipWithIndex.map { (board, z) =>
      board.zipWithIndex.flatMap { (line, y) =>
          line.trim.split("\\s+").zipWithIndex.map { (num, x) =>
            Position(x, y) -> num.toInt
        }
      }.toMap
    }.map(Board(_, List.empty))

    Game(numbers, boards)

  @tailrec
  def play1(game: Game): (Board, Int) =
    val (nextGame, num) = game.take
    nextGame.winners match {
      case Seq(winner) => (winner, num)
      case _ => play1(nextGame)
    }

  @tailrec
  def play2(game: Game): (Board, Int) =
    val (nextGame, num) = game.take
    nextGame.winners match {
      case Seq(winner) if (nextGame.boards.size == 1) => (winner, num)
      case winners if (nextGame.boards.size > 1) => play2(nextGame.removeAll(winners))
      case _ => play2(nextGame)
    }

  def part1(input: String): Int =
    val game = parse(input)
    val (board, num) = play1(game)
    board.score * num

  def part2(input: String): Int =
    val game = parse(input)
    val (board, num) = play2(game)
    board.score * num

@main def main4: Unit = 
  val input = Source.fromFile("input/day4.txt").mkString

  println(s"Day4 part1: ${Day4.part1(input)}")
  println(s"Day4 part2: ${Day4.part2(input)}")