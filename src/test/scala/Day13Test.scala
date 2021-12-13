import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day13Test:

  val input = """6,10
                |0,14
                |9,10
                |0,3
                |10,4
                |4,11
                |6,0
                |6,12
                |4,1
                |0,13
                |10,12
                |3,4
                |3,0
                |8,4
                |1,10
                |2,14
                |8,10
                |9,0
                |
                |fold along y=7
                |fold along x=5""".stripMargin

  @Test def part1(): Unit = 
    assertEquals(17, Day13.part1(input))

  @Test def part2(): Unit = 
    assertEquals(0, Day13.part2(input))