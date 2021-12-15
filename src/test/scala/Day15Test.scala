import org.junit.Test
import org.junit.Assert.*

class Day15Test:

  val input = """1163751742
                |1381373672
                |2136511328
                |3694931569
                |7463417111
                |1319128137
                |1359912421
                |3125421639
                |1293138521
                |2311944581""".stripMargin.split("\n").toList

  @Test def part1(): Unit = 
    assertEquals(40, Day15.part1(input))

  @Test def part2(): Unit = 
    assertEquals(315, Day15.part2(input))