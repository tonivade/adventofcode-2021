import org.junit.Test
import org.junit.Assert.*

class Day2Test:

  val list = List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

  @Test def part1(): Unit = 
    assertEquals(150, Day2.part1(list))

