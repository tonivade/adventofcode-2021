import scala.io.Source
import scala.collection.mutable

object Day14:

  def parseMappings(input: List[String]): Map[(Char, Char), Char] = 
    val mappingregex = """(\w\w) -> (\w)""".r
    input.map {
      case mappingregex(left, right) => ((left(0), left(1)), right(0))
    }.toMap

  def step(seed: String, mapping: Map[(Char, Char), Char]): String = 
    val seq = seed.dropRight(1) zip seed.tail
    seq.map {
      case (a, b) => s"$a${mapping(a, b)}"
    }.mkString + seed.last

  def step2(pairs: Map[(Char, Char), Long], mapping: Map[(Char, Char), Char]): Map[(Char, Char), Long] = 
    val m = mutable.Map.from(pairs)
    pairs.foreach {
      case ((a, b), c) => 
        m.updateWith(a, mapping(a, b))(x => x.map(_ + c).orElse(Some(c)))
        m.updateWith(mapping(a, b), b)(x => x.map(_ + c).orElse(Some(c)))
        m.updateWith(a, b)(x => x.map(_ - c))
    }
    Map.from(m).filter {
      case (_, 0) => false
      case _ => true
    }

  def go(n: Int, seed: String, mapping: Map[(Char, Char), Char]): String =
    if (n > 0)
      val next = step(seed, mapping)
      go(n - 1, next, mapping)
    else
      seed

  def go2(n: Int, pairs: Map[(Char, Char), Long], mapping: Map[(Char, Char), Char]): Map[(Char, Char), Long] =
    if (n > 0)
      val next = step2(pairs, mapping)
      go2(n - 1, next, mapping)
    else
      pairs

  def parse(input: String): (String, Map[(Char, Char), Char]) =
    input.split("\n\n") match {
      case Array(s, m) => (s, parseMappings(m.split("\n").toList))
    }

  def pairs(input: String): Map[(Char, Char), Long] =
    (input.dropRight(1) zip input.tail).groupBy(identity).mapValues(_.size.toLong).toMap

  def part1(input: String): Int = 
    val (seed, mapping) = parse(input)
    val polymer = go(10, seed, mapping)
    val total = polymer.groupBy(identity).mapValues(_.size).toMap
    val max = total.values.max
    val min = total.values.min
    max - min

  def part2(input: String): Long =
    val (seed, mapping) = parse(input)
    val polymer = go2(40, pairs(seed), mapping)
    val total = polymer.foldLeft(Map.empty[Char, Long]) {
      case (map, ((a, b), c)) => 
        map.updatedWith(a)(o => o.map(_ + (c / 2)).orElse(Some((c / 2))))
           .updatedWith(b)(o => o.map(_ + (c / 2)).orElse(Some((c / 2))))
    }
    val max = total.values.toList.max
    val min = total.values.toList.min
    max - min

@main def main14: Unit = 
  val input = Source.fromFile("input/day14.txt").mkString

  println(s"Day14 part1: ${Day14.part1(input)}")
  println(s"Day14 part2: ${Day14.part2(input)}")
  