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
      case (state, Forward(x)) => (state._1 + x, state._2)
      case (state, Up(y)) => (state._1, state._2 + y)
      case (state, Down(y)) => (state._1, state._2 - y)
    }.toList.foldLeft(1)(_ * _).abs
}

@main def main2: Unit = 
  val input = Source.fromFile("input/day2.txt").getLines.toList

  println(s"Day2 part1: ${Day2.part1(input)}")