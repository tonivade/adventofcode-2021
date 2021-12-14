import scala.io.Source

object Day14:

  def parseMappings(input: List[String]): Map[String, String] = 
    val mappingregex = """(\w\w) -> (\w)""".r
    input.map {
      case mappingregex(left, right) => (left, right)
    }.toMap

  def step(seed: String, mapping: Map[String, String]): String = 
    val seq = seed.dropRight(1) zip seed.tail
    seq.map {
      case (a, b) => a.toString + mapping(Array(a, b).mkString)
    }.mkString + seed.last

  def go(n: Int, seed: String, mapping: Map[String, String]): String =
    if (n > 0)
      val next = step(seed, mapping)
      go(n - 1, next, mapping)
    else
      seed

  def parse(input: String): (String, Map[String, String]) =
    input.split("\n\n") match {
      case Array(s, m) => (s, parseMappings(m.split("\n").toList))
    }

  def part1(input: String): Int = 
    val (seed, mapping) = parse(input)
    val polymer = go(10, seed, mapping)
    val grouped = polymer.groupBy(identity).mapValues(_.size).toMap
    val max = grouped.maxBy(_._2)._2
    val min = grouped.minBy(_._2)._2
    max - min

  def part2(input: String): Int = ???

@main def main14: Unit = 
  val input = Source.fromFile("input/day14.txt").mkString

  println(s"Day14 part1: ${Day14.part1(input)}")
  println(s"Day14 part2: ${Day14.part2(input)}")
  