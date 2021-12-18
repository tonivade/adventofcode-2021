import scala.io.Source
import scala.annotation.tailrec

object Day18:

  sealed trait Node
  case class Leaf(value: Int) extends Node
  case class Pair(left: Node, right: Node) extends Node

  def parseNode(input: String): (Node, String) =
    input.head match {
      case '[' => parsePair(input.tail)
      case digit => (Leaf(digit.asDigit), input.tail)
    }

  def parsePair(input: String): (Node, String) =
    val (left, first) = parseNode(input)
    val (right, second) = parseNode(first.tail)
    (Pair(left, right), second.tail)

  def parse(input: String): Node = parseNode(input)._1

  def part1(input: String): Int = ???

  def part2(input: String): Int = ???

@main def main18: Unit = 
  val input = Source.fromFile("input/day18.txt").getLines.next

  println(s"Day18 part1: ${Day18.part1(input)}")
  println(s"Day18 part2: ${Day18.part2(input)}")
  