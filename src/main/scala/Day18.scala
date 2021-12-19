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

    def path(target: Node, path: List[Node] = Nil): Option[List[Node]] =
      this match {
        case current if (current eq target) => Some(path)
        case Pair(left, right) => 
          left.path(target, this :: path) orElse right.path(target, this :: path)
        case _ => None
      }

    def updateLeft(toUpdate: Node, value: Int): Node =
      this match {
        case Pair(current, right) if (current eq toUpdate) => Pair(current, right.addToLeft(value))
        case Pair(left, right) => Pair(left.updateLeft(toUpdate, value), right.updateLeft(toUpdate, value))
        case _ => this
      }

    def updateRight(toUpdate: Node, value: Int): Node =
      this match {
        case Pair(left, current) if (current eq toUpdate) => Pair(left.addToRight(value), current)
        case Pair(left, right) => Pair(left.updateRight(toUpdate, value), right.updateRight(toUpdate, value))
        case _ => this
      }
    
    def explode(toExplode: Pair): Node =
      val (l, n, r) = _explode(toExplode)
      if (l > 0)
        val pathToExplode = path(toExplode).toList.flatMap(identity)
        val found = pathToExplode.flatMap { node =>
          def helper(n: Node): Option[Node] = 
            n match {
              case Pair(left, _) if (left eq toExplode) => None
              case Pair(left, _) => helper(left)
              case current => Some(current)
            }
          helper(node).toList
        }
        found match {
          case toUpdate :: tail => n.updateLeft(toUpdate, l)
          case _ => n
        }
      else if (r > 0)
        val pathToExplode = path(toExplode).toList.flatMap(identity)
        val found = pathToExplode.flatMap { node =>
          def helper(n: Node): Option[Node] = 
            n match {
              case Pair(_, right) if (right eq toExplode) => None
              case Pair(_, right) => helper(right)
              case current => Some(current)
            }
          helper(node).toList
        }
        found match {
          case toUpdate :: tail => n.updateRight(toUpdate, r)
          case _ => n
        }
      else n

    def _explode(toExplode: Pair): (Int, Node, Int) =
      this match {
        case Pair(left, right) if (left eq toExplode) => 
          (toExplode.left.asInstanceOf[Leaf].value,
            Pair(Leaf(0), right.addToLeft(toExplode.right.asInstanceOf[Leaf].value)),
              0)
        case Pair(left, right) if (right eq toExplode) => 
          (0, 
            Pair(left.addToRight(toExplode.left.asInstanceOf[Leaf].value), Leaf(0)), 
              toExplode.right.asInstanceOf[Leaf].value)
        case Pair(left, right) => 
          val (ll, l, lr) = left._explode(toExplode)
          val (rl, r, rr) = right._explode(toExplode)
          (ll + rl, Pair(l, r), lr + rr)
        case _ => (0, this, 0)
      }

    def addToLeft(value: Int): Node = 
      this match {
        case Leaf(other) => Leaf(other + value)
        case Pair(left, right) => Pair(left.addToLeft(value), right)
      }

    def addToRight(value: Int): Node =
      this match {
        case Leaf(other) => Leaf(other + value)
        case Pair(left, right) => Pair(left, right.addToRight(value))
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
      case Pair(Leaf(_), Leaf(_)) if (depth == 4) => Some(Explode(node.asInstanceOf[Pair]))
      case Pair(left, right) => toExplode(left, depth + 1) orElse toExplode(right, depth + 1)
      case _ => None
    }

  @tailrec
  def reduce(node: Node): Node =
    val action = toExplode(node) orElse toSplit(node)
    action match {
      case Some(Split(leaf)) => reduce(node.split(leaf))
      case Some(Explode(pair)) => reduce(node.explode(pair))
      case None => node
    }

  def add(a: Node, b: Node): Node = reduce(a add b)

  def addAll(input: List[String]): Node = input.map(parse).reduce(add)

  def part1(input: List[String]): Int = 
    1

  def part2(input: List[String]): Int = ???

@main def main18: Unit = 
  val input = Source.fromFile("input/day18.txt").getLines.toList

  println(s"Day18 part1: ${Day18.part1(input)}")
  println(s"Day18 part2: ${Day18.part2(input)}")
  