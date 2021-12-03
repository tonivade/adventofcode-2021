import scala.io.Source

object Day3:
  def binaryToInt(binary: String): Int = 
    binary.reverse.zipWithIndex.map((x, i) => { if (x == '1') 1 << i else 0 }).sum

  def part1(input: List[String]): Int = 
    val counted = (0 until input(0).size)
      .map { i => input.map(_(i)) }
      .map { _.groupBy(identity) }
      .map { _.values }
    
    val gamma = counted
      .map { _.maxBy(_.size) }
      .map { _.max }
      .mkString

    val epsilon = counted
      .map { _.minBy(_.size) }
      .map { _.min }
      .mkString

    binaryToInt(gamma) * binaryToInt(epsilon)

  def part2(input: List[String]): Int = ???

@main def main3: Unit = 
  val input = Source.fromFile("input/day3.txt").getLines.toList

  println(s"Day3 part1: ${Day3.part1(input)}")
  println(s"Day3 part2: ${Day3.part2(input)}")