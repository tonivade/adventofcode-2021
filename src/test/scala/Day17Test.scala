import org.junit.Test
import org.junit.Assert.*

class Day17Test:

  val input: String = "target area: x=20..30, y=-10..-5"

  @Test def part1(): Unit = 
    assertEquals(0, Day17.part1(input))

  @Test def part20(): Unit = 
    assertEquals(0, Day17.part2(input))