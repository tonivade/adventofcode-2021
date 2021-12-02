import scala.io.Source

object Day2 {

  sealed trait Move
  case class Forward(x: Int) extends Move
  case class Up(y: Int) extends Move
  case class Down(y: Int) extends Move

  def parse(input: List[String]): List[Move] = 
    input.map(s => s.split(" ")).map({
      case Array("forward", x) => Forward(x.toInt)
      case Array("up", x) => Up(x.toInt)
      case Array("down", x) => Down(x.toInt)
    })

  def part1(input: List[String]): Int = 
    parse(input).foldLeft((0, 0)) {
      case ((x, y), Forward(x1)) => (x + x1, y)
      case ((x, y), Up(y1)) => (x, y + y1)
      case ((x, y), Down(y1)) => (x, y - y1)
    }.toList.foldLeft(1)(_ * _).abs

  def part2(input: List[String]): Int = 
    parse(input).foldLeft((0, 0, 0)) {
      case ((x, y, z), Forward(x1)) => (x + x1, y + (z * x1), z)
      case ((x, y, z), Up(z1)) => (x, y, z + z1)
      case ((x, y, z), Down(z1)) => (x, y, z - z1)
    }.toList.dropRight(1).foldLeft(1)(_ * _).abs
}

@main def main2: Unit = 
  val input = Source.fromFile("input/day2.txt").getLines.toList

  println(s"Day2 part1: ${Day2.part1(input)}")
  println(s"Day2 part2: ${Day2.part2(input)}")