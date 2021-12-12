import scala.io.Source

object Day12:

  sealed trait Node
  case object Start extends Node
  case object End extends Node
  case class BigCave(name: String) extends Node
  case class SmallCave(name: String) extends Node

  case class Graph(connections: Map[Node, List[Node]]):
    def go(to: Node): List[Node] = connections(to)
    def searchFrom(from: Node, path: List[Node] = List.empty): List[List[Node]] = ???

  def parse(input: List[String]): Graph =
    val direct: List[(Node, Node)] = input.map(_.split("-")).map {
     case Array("start", "end") => Start -> End
     case Array("end", "start") => End -> Start
     case Array("start", right) if (right.forall(_.isUpper)) => Start -> BigCave(right)
     case Array("start", right) if (right.forall(_.isLower)) => Start -> SmallCave(right)
     case Array("end", right) if (right.forall(_.isUpper)) => End -> BigCave(right)
     case Array("end", right) if (right.forall(_.isLower)) => End -> SmallCave(right)
     case Array(left, "start") if (left.forall(_.isUpper)) => BigCave(left) -> Start
     case Array(left, "start") if (left.forall(_.isLower)) => SmallCave(left) -> Start
     case Array(left, "end") if (left.forall(_.isUpper)) => BigCave(left) -> End
     case Array(left, "end") if (left.forall(_.isLower)) => SmallCave(left) -> End
     case Array(left, right) if (left.forall(_.isUpper) && right.forall(_.isUpper)) => BigCave(left) -> BigCave(right)
     case Array(left, right) if (left.forall(_.isLower) && right.forall(_.isLower)) => SmallCave(left) -> SmallCave(right)
     case Array(left, right) if (left.forall(_.isUpper) && right.forall(_.isLower)) => BigCave(left) -> SmallCave(right)
     case Array(left, right) if (left.forall(_.isLower) && right.forall(_.isUpper)) => SmallCave(left) -> BigCave(right)
    }
    var reverse = direct.map(_.swap)
    val all = direct ++ reverse

    Graph(all.groupBy(_._1).mapValues(_.map(_._2)).toMap)

  def part1(input: List[String]): Int = parse(input).searchFrom(Start).size

  def part2(input: List[String]): Int = ???

@main def main12: Unit = 
  val input = Source.fromFile("input/day12.txt").getLines.toList

  println(s"Day12 part1: ${Day12.part1(input)}")
  println(s"Day12 part2: ${Day12.part2(input)}")