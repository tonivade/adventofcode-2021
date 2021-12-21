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
      doExplode(toExplode) match {
        case (left, newNode, _) if (left > 0) =>
          searchLeft(toExplode).map(_.leafToLeft) match {
            case Some(node) => newNode.update(node, left)
            case _ => newNode
          }
        case (_, newNode, right) if (right > 0) =>
          searchRight(toExplode).map(_.leafToRight) match {
            case Some(node) => newNode.update(node, right)
            case _ => newNode
          }
        case (_, newNode, _) => newNode
      }

    def leafToLeft: Leaf =
      this match {
        case leaf: Leaf => leaf
        case Pair(_, right) => right.leafToLeft
      }

    def leafToRight: Leaf =
      this match {
        case leaf: Leaf => leaf
        case Pair(left, _) => left.leafToRight
      }

    def searchLeft(target: Node, path: List[Node] = Nil): Option[Node] =
      this match {
        case Pair(left, right) if (left eq target) => 
          @tailrec
          def go(current: Node, path: List[Node]): Option[Node] =
            path match {
              case Pair(left, _) :: tail if (current ne left) => Some(left)
              case head :: tail => go(head, tail)
              case _ => None
            }
          go(this, path)
        case Pair(left, right) => left.searchLeft(target, this :: path) orElse right.searchLeft(target, this :: path)
        case _ => None
      }

    def searchRight(target: Node, path: List[Node] = Nil): Option[Node] =
      this match {
        case Pair(left, right) if (right eq target) => 
          @tailrec
          def go(current: Node, path: List[Node]): Option[Node] =
            path match {
              case Pair(_, right) :: tail if (current ne right) => Some(right)
              case head :: tail => go(head, tail)
              case _ => None
            }
          go(this, path)
        case Pair(left, right) => left.searchRight(target, this :: path) orElse right.searchRight(target, this :: path)
        case _ => None
      }

    def update(toUpdate: Node, value: Int): Node =
      this match {
        case current: Leaf if (current eq toUpdate) => Leaf(current.value + value)
        case Pair(left, right) => Pair(left.update(toUpdate, value), right.update(toUpdate, value))
        case _ => this
      }

    private def doExplode(toExplode: Pair): (Int, Node, Int) =
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
          val (ll, l, lr) = left.doExplode(toExplode)
          val (rl, r, rr) = right.doExplode(toExplode)
          (ll + rl, Pair(l, r), lr + rr)
        case _ => (0, this, 0)
      }

    private def addToLeft(value: Int): Node = 
      this match {
        case Leaf(other) => Leaf(other + value)
        case Pair(left, right) => Pair(left.addToLeft(value), right)
      }

    private def addToRight(value: Int): Node =
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

  def magnitude(node: Node): Int =
    node match {
      case Leaf(value) => value
      case Pair(left, right) => (3 * magnitude(left)) + (2 * magnitude(right))
    }

  def part1(input: List[String]): Int = 
    val result = addAll(input)
    magnitude(result)

  def part2(input: List[String]): Int = 
    val parsed = input.map(parse)
    val all = for {
      a <- parsed
      b <- parsed
    } yield(a, b)
    val combinations = all.filter((a, b) => a != b).map(_.toList)
    combinations.map(_.reduce(add)).map(magnitude).max

@main def main18: Unit = 
  val input = Source.fromFile("input/day18.txt").getLines.toList

  println(s"Day18 part1: ${Day18.part1(input)}")
  println(s"Day18 part2: ${Day18.part2(input)}")
  