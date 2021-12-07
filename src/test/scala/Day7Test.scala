import org.junit.Test
import org.junit.Assert.*

class Day7Test:

  val input = "16,1,2,0,4,2,7,1,2,14"

  @Test def part1(): Unit = 
    assertEquals(37, Day7.part1(input))

  @Test def part2(): Unit =
    assertEquals(168, Day7.part2(input))

  @Test def distance2To(): Unit = {
    assertEquals(0, Day7.distance2To(1)(1))
    assertEquals(1, Day7.distance2To(1)(2))
    assertEquals(3, Day7.distance2To(1)(3))
    assertEquals(6, Day7.distance2To(1)(4))
  }
