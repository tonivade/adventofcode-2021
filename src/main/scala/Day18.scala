import scala.io.Source
import scala.annotation.tailrec

object Day18:

  sealed trait Node:

    def add(other: Node): Node = Pair(this, other)

    def split(toSplit: Leaf): Node = 
      this match {
        case Leaf(value) if (this eq toSplit) => 
          val l = value / 2
          val r = (value / 2) + (value % 2)
          Pair(Leaf(l), Leaf(r))
        case Pair(left, right) => Pair(left.split(toSplit), right.split(toSplit))
        case _ => this
      }

    def explode(toExplode: Pair): Node =
      this match {
        case Pair(left, right) if (left eq toExplode) => ???
        case Pair(left, right) if (right eq toExplode) => ???
        case Pair(left, right) => Pair(left.explode(toExplode), right.explode(toExplode))
        case _ => this
      }

  case class Leaf(value: Int) extends Node
  case class Pair(left: Node, right: Node) extends Node

  sealed trait Action
  case class Split(leaf: Leaf) extends Action
  case class Explode(pair: Pair) extends Action

  def parse(input: String): Node = parseNode(input)._1

  def parseNode(input: String): (Node, String) =
    input.head match {
      case '[' => parsePair(input.tail)
      case digit => (Leaf(digit.asDigit), input.tail)
    }

  def parsePair(input: String): (Node, String) =
    val (left, first) = parseNode(input)
    val (right, second) = parseNode(first.tail)
    (Pair(left, right), second.tail)

  def toSplit(node: Node): Option[Action] = 
    node match {
      case Leaf(value) if (value > 9) => Some(Split(node.asInstanceOf[Leaf]))
      case Pair(left, right) => toSplit(left) orElse toSplit(right)
      case _ => None
    }

  def toExplode(node: Node, depth: Int = 0): Option[Action] = 
    node match {
      case Pair(left, right) if (depth == 4) => Some(Explode(node.asInstanceOf[Pair]))
      case Pair(left, right) => toExplode(left, depth + 1) orElse toExplode(right, depth + 1)
      case _ => None
    }

  def reduce(node: Node): Node =
    val action = toExplode(node) orElse toSplit(node)
    action match {
      case Some(Split(leaf)) => node.split(leaf)
      case Some(Explode(pair)) => node.explode(pair)
      case None => node
    }

  def part1(input: String): Int = ???

  def part2(input: String): Int = ???

@main def main18: Unit = 
  val input = Source.fromFile("input/day18.txt").getLines.next

  println(s"Day18 part1: ${Day18.part1(input)}")
  println(s"Day18 part2: ${Day18.part2(input)}")
  