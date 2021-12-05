import org.junit.Test
import org.junit.Assert.*

class Day5Test:

  val input = """0,9 -> 5,9
                |8,0 -> 0,8
                |9,4 -> 3,4
                |2,2 -> 2,1
                |7,0 -> 7,4
                |6,4 -> 2,0
                |0,9 -> 2,9
                |3,4 -> 1,4
                |0,0 -> 8,8
                |5,5 -> 8,2""".stripMargin

  @Test def part1(): Unit = 
    assertEquals(5, Day5.part1(input))

  @Test def part2(): Unit =
    assertEquals(12, Day5.part2(input))
