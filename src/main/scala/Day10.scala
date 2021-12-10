import scala.io.Source
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Day10:

  def parseLine(line: String): Option[Char] = 
    val parse = Try(line.foldLeft(List.empty[Char]) {
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
    parse match {
      case Success(_) => None
      case Failure(e) => Some(e.getMessage()(0))
    }

  def part1(input: List[String]): Int =
    val translation = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )
    input.map(parseLine).flatMap(_.toList).map(translation).sum

  def part2(input: List[String]): Int = ???

@main def main10: Unit = 
  val input = Source.fromFile("input/day10.txt").getLines.toList

  println(s"Day10 part1: ${Day10.part1(input)}")
  println(s"Day10 part2: ${Day10.part2(input)}")