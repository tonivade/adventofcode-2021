import scala.io.Source
import scala.annotation.tailrec

object Day3:
  def binaryToInt(binary: String): Int = 
    binary.reverse.zipWithIndex.map((x, i) => if (x == '1') 1 << i else 0).sum

  def countBits(input: List[String]): Seq[Iterable[List[Char]]] = 
    (0 until input(0).size)
      .map { i => input.map(_(i)) }
      .map { _.groupBy(identity) }
      .map { _.values }

  def part1(input: List[String]): Int = 
    val counted = countBits(input)
    
    val gamma = counted
      .map { _.maxBy(_.size) }
      .map { _.max }
      .mkString

    val epsilon = counted
      .map { _.minBy(_.size) }
      .map { _.min }
      .mkString

    binaryToInt(gamma) * binaryToInt(epsilon)

  @tailrec
  def calculateO2(input: List[String], y: Int = 0): Int =
    if (input.size == 1) 
      input.map(binaryToInt)(0)
    else
      val counted = countBits(input)
      val c = counted(y).toList.map(_.size) match {
        case List(i, j) if (i > j) => '1'
        case List(i, j) if (i < j) => '0'
        case _ => '1'
      }
      calculateO2(input.filter(_(y) == c), y + 1)

  @tailrec
  def calculateCO2(input: List[String], y: Int = 0): Int =
    if (input.size == 1) 
      input.map(binaryToInt)(0)
    else
      val counted = countBits(input)
      val c = counted(y).toList.map(_.size) match {
        case List(i, j) if (i > j) => '0'
        case List(i, j) if (i < j) => '1'
        case _ => '0'
      }
      calculateCO2(input.filter(_(y) == c), y + 1)

  def part2(input: List[String]): Int =
    calculateO2(input) * calculateCO2(input)

@main def main3: Unit = 
  val input = Source.fromFile("input/day3.txt").getLines.toList

  println(s"Day3 part1: ${Day3.part1(input)}")
  println(s"Day3 part2: ${Day3.part2(input)}")