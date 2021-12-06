import org.junit.Test
import org.junit.Assert.*

class Day6Test:

  val input = "3,4,3,1,2"

  @Test def part1(): Unit = 
    assertEquals(5934, Day6.part1(input))

  @Test def part2(): Unit =
    assertEquals(26984457539L, Day6.part2(input))
