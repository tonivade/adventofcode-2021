import scala.io.Source

object Day13:

  sealed trait Fold
  case class FoldX(x: Int) extends Fold
  case class FoldY(y: Int) extends Fold

  def parseDots(input: String): List[(Int, Int)] = 
    input.split("\n").map(_.split(",")).map {
      case Array(x, y) => (x.toInt, y.toInt)
    }.toList

  def parseFolds(input: String): List[Fold] = 
    val foldx = """fold along x=(\d+)""".r
    val foldy = """fold along y=(\d+)""".r
    input.split("\n").map {
      case foldx(value) => FoldX(value.toInt)
      case foldy(value) => FoldY(value.toInt)
    }.toList

  def printDots(dots: List[(Int, Int)]): List[List[Int]] =
    val maxX = dots.map(_._1).max + 1
    val maxY = dots.map(_._2).max + 1
    dots.foldLeft(List.fill(maxY, maxX)(0)) {
      case (matrix, (x, y)) => matrix.updated(y, matrix(y).updated(x, 1))
    }

  def mergeY(top: List[List[Int]], bottom: List[List[Int]]): List[List[Int]] =
    val reverse = bottom.reverse
    top.zipWithIndex.map {
      (line, y) => line.zipWithIndex.map {
        (value, x) => value + reverse(y)(x)
      }
    }
  
  def mergeX(left: List[List[Int]], right: List[List[Int]]): List[List[Int]] = 
    val reverse = right.map(_.reverse)
    left.zipWithIndex.map {
      (line, y) => line.zipWithIndex.map {
        (value, x) => value + reverse(y)(x)
      }
    }
  
  def applyFold(map: List[List[Int]], fold: Fold): List[List[Int]] =
    fold match {
      case FoldX(x) => 
        val s = map.map(_.splitAt(x))
        mergeX(s.map(_._1), s.map(_._2.tail))
      case FoldY(y) => 
        val s = map.splitAt(y)
        mergeY(s._1, s._2.tail)
    }

  def part1(input: String): Int = 
    val (dots, folds) = input.split("\n\n") match {
      case Array(d, f) => (parseDots(d), parseFolds(f))
    }
    val map = printDots(dots)
    val firstFold = applyFold(map, folds(0))
    firstFold.flatMap(identity).count(_ > 0)

  def part2(input: String): String = 
    val (dots, folds) = input.split("\n\n") match {
      case Array(d, f) => (parseDots(d), parseFolds(f))
    }
    val map = printDots(dots)

    val result = folds.foldLeft(map) {
      (m, f) => applyFold(m, f)
    }

    result.map(_.map {
      case 0 => '.'
      case _ => '#'
    }).map(_.mkString).mkString("\n")

@main def main13: Unit = 
  val input = Source.fromFile("input/day13.txt").mkString

  println(s"Day13 part1: ${Day13.part1(input)}")
  println(s"Day13 part2: \n${Day13.part2(input)}")
  