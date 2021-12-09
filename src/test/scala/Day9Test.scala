import org.junit.Test
import org.junit.Assert.*

class Day9Test:

  val input = """2199943210
                |3987894921
                |9856789892
                |8767896789
                |9899965678""".stripMargin.split("\n").toList

  @Test def part1(): Unit = 
    assertEquals(15, Day9.part1(input))

  @Test def part2(): Unit =
    assertEquals(1134, Day9.part2(input))
