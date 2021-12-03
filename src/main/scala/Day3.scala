import scala.io.Source

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

  def calculateO2(input: List[String]): Int =
    var x = input
    var y = 0
    while (x.size > 1) {
      val counted = countBits(x).toList
      var c = counted(y).toList.map(_.size) match {
        case List(i, j) => if (i > j) '1' else if (i < j) '0' else '1'
      }

      x = x.filter(_(y) == c)
      y = y + 1
    }
    x.map(binaryToInt)(0)

  def calculateCO2(input: List[String]): Int =
    var x = input
    var y = 0
    while (x.size > 1) {
      val counted = countBits(x).toList
      var c = counted(y).toList.map(_.size) match {
        case List(i, j) => if (i > j) '0' else if (i < j) '1' else '0'
      }

      x = x.filter(_(y) == c)
      y = y + 1
    }
    x.map(binaryToInt)(0)

  def part2(input: List[String]): Int =

    val o2 = calculateO2(input)
    val co2 = calculateCO2(input)

    o2 * co2

@main def main3: Unit = 
  val input = Source.fromFile("input/day3.txt").getLines.toList

  println(s"Day3 part1: ${Day3.part1(input)}")
  println(s"Day3 part2: ${Day3.part2(input)}")