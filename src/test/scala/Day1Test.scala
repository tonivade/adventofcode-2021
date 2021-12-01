import org.junit.Test
import org.junit.Assert.*

class Day1Test:

  val list = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  @Test def part1(): Unit = 
    assertEquals(7, Day1.part1(list))

  @Test def part2(): Unit = 
    assertEquals(5, Day1.part2(list))
