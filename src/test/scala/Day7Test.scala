import org.junit.Test
import org.junit.Assert.*

class Day7Test:

  val input = "16,1,2,0,4,2,7,1,2,14"

  @Test def part1(): Unit = 
    assertEquals(37, Day7.part1(input))

  @Test def part2(): Unit =
    assertEquals(26984457539L, Day7.part2(input))
