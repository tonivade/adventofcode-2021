import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day13Test:

  val input = """""".stripMargin.split("\n").toList

  @Test def part1(): Unit = 
    assertEquals(0, Day13.part1(input))

  @Test def part2(): Unit = 
    assertEquals(0, Day13.part2(input))