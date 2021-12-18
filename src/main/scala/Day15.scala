import scala.io.Source
import java.util.PriorityQueue
import scala.collection.mutable
import scala.annotation.tailrec

object Day15:

  case class Point(x: Int, y: Int):
    def adjacent: List[Point] =
      List(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1))

  class Node(val point: Point, var parent: Point, var distance: Int) extends Comparable[Node]:
    def compareTo(other: Node): Int = this.distance - other.distance

  def search(points: Map[Point, Int], routes: Map[Point, Map[Point, Int]]): Map[Point, Node] =
    val queue = PriorityQueue[Node]()
    points.map {
      case (Point(0, 0), int) => Node(Point(0, 0), null, 0)
      case (p, int) => Node(p, null, Int.MaxValue)
    }.foreach(queue.add)

    val index = mutable.Map[Point, Node]()
    queue.forEach {
      node => index.put(node.point, node)
    }

    while(!queue.isEmpty) {
      val node = queue.remove()
      val edges = routes(node.point)

      edges.foreach {
        (to, cost) => 
          val distance = node.distance + cost
          val other = index(to)
          if (distance < other.distance) {
            other.distance = distance
            other.parent = node.point
            queue.remove(other)
            queue.add(other)
          }
      }
    }

    Map.from(index)

  def part1(input: List[String]): Int = 
    val grid = input.map(_.map(_.asDigit))

    val width = grid(0).size
    val height = grid.size

    val all = for {
      x <- 0 until width
      y <- 0 until height
    } yield Point(x, y)

    val points = grid.zipWithIndex.flatMap {
      (line, y) => line.zipWithIndex.map {
        (value, x) => (Point(x, y), value)
      }
    }.toMap

    val origin = Point(0, 0)
    val destination = Point(width - 1, height - 1)

    val routes = points.map {
      case (from, _) => (from, from.adjacent.filter(points.contains).map {
        to => (to, points(to))
      }.toMap)
    }

    val graph = search(points, routes)

    def go(point: Point): List[Point] =
      val node = graph(point)
      if (node.parent != null)
        node.point :: go(node.parent)
      else
        Nil

    val path = go(destination)

    path.map(points).sum

  def part2(input: List[String]): Int =
    val grid = input.map(_.map(_.asDigit))

    val col = (0 until 5).flatMap {
      y => grid.map {
        line => line.map {
          i => 
            val j = i + y
            if (j > 9)
              j - 9
            else 
              j 
        }
      }
    }

    val rows = col.map {
      line => (0 until 5).flatMap {
        x => line.map {
          i => 
            val j = i + x
            if (j > 9)
              j - 9
            else
              j
        }
      }
    }

    part1(rows.map(_.mkString).toList)

@main def main15: Unit = 
  val input = Source.fromFile("input/day15.txt").getLines.toList

  println(s"Day15 part1: ${Day15.part1(input)}")
  println(s"Day15 part2: ${Day15.part2(input)}")
  