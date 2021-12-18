import scala.io.Source
import scala.annotation.tailrec

object Day17:

  type Position = (Int, Int)
  type Speed = (Int, Int)
  
  case class Target(x1: Int, x2: Int, y1: Int, y2: Int):
    def contains(probe: Probe): Boolean =
      probe.position match {
        case (x, y) => x >= x1 && x <= x2 && y >= y1 && y <= y2
      }
    def missed(probe: Probe): Boolean = 
      probe.position match {
        case (x, y) => x > x2 || y < y1
      }

  case class Probe(position: Position, speed: Speed, path: List[(Int, Int)] = Nil):
    def maxY: Int = path.map(_._2).max

  def step(probe: Probe): Probe =
    probe match {
      case Probe((px, py), (sx, sy), path) if (sx > 0) => 
        Probe((px + sx, py + sy), (sx - 1, sy - 1), (px, py) :: path)
      case Probe((px, py), (sx, sy), path) if (sx < 0) => 
        Probe((px + sx, py + sy), (sx + 1, sy - 1), (px, py) :: path)
      case Probe((px, py), (sx, sy), path) if (sx == 0) => 
        Probe((px + sx, py + sy), (sx, sy - 1), (px, py) :: path)
    }

  @tailrec
  def launch(target: Target)(probe: Probe): Option[Probe] =
    if (target.contains(probe))
      Some(probe)
    else if (target.missed(probe))
      None
    else
      val nextProbe = step(probe)
      launch(target)(nextProbe)

  def search(input: String): Seq[Probe] = 
    val targetregex = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".r
    val target = input match {
      case targetregex(x1, x2, y1, y2) => Target(x1.toInt, x2.toInt, y1.toInt, y2.toInt)
    }
    val all = for {
      x <- 0 to target.x2
      y <- target.y1 to target.y1.abs
    } yield (x, y)

    all.map(Probe((0, 0), _)).map(launch(target)).flatMap(_.toList)

  def part1(input: String): Int =
    search(input).map(_.maxY).max

  def part2(input: String): Int = 
    search(input).size

@main def main17: Unit = 
  val input = Source.fromFile("input/day17.txt").getLines.next

  println(s"Day17 part1: ${Day17.part1(input)}")
  println(s"Day17 part2: ${Day17.part2(input)}")
  