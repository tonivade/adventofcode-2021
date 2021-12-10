import scala.io.Source
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Day10:

  def parse(line: String): Try[List[Char]] =
    Try(line.foldLeft(List.empty[Char]) {
      case (stack, '(') => '(' :: stack
      case (stack, '[') => '[' :: stack
      case (stack, '{') => '{' :: stack
      case (stack, '<') => '<' :: stack

      case (head :: tail, ')') if (head == '(') => tail
      case (head :: tail, ']') if (head == '[') => tail
      case (head :: tail, '}') if (head == '{') => tail
      case (head :: tail, '>') if (head == '<') => tail

      case (state, c) => throw new IllegalArgumentException(c.toString)
    })

  def parseLine1(line: String): Option[Char] = 
    parse(line) match {
      case Success(l) => None
      case Failure(e) => Some(e.getMessage()(0))
    }

  def part1(input: List[String]): Int =
    val translation = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )
    input.map(parseLine1).flatMap(_.toList).map(translation).sum

  def part2(input: List[String]): Long =
    val r = input.map(parse).flatMap(_.toOption.toList).map {
      _.foldLeft(0L) {
        case (total, '(') => (total * 5) + 1L
        case (total, '[') => (total * 5) + 2L
        case (total, '{') => (total * 5) + 3L
        case (total, '<') => (total * 5) + 4L
      }
    }

    r.sorted.drop(r.size / 2).head

@main def main10: Unit = 
  val input = Source.fromFile("input/day10.txt").getLines.toList

  println(s"Day10 part1: ${Day10.part1(input)}")
  println(s"Day10 part2: ${Day10.part2(input)}")