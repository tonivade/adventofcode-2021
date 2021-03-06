import org.junit.Test
import org.junit.Assert.*

class Day11Test:

  val input = """5483143223
                |2745854711
                |5264556173
                |6141336146
                |6357385478
                |4167524645
                |2176841721
                |6882881134
                |4846848554
                |5283751526""".stripMargin.split("\n").toList

  @Test def part1(): Unit = 
    assertEquals(1656, Day11.part1(input))

  @Test def part1step(): Unit = 
    val start = Day11.Board(List(
      List(1, 1, 1, 1, 1),
      List(1, 9, 9, 9, 1),
      List(1, 9, 1, 9, 1),
      List(1, 9, 9, 9, 1),
      List(1, 1, 1, 1, 1),
    ))

    val step1 = Day11.Board(List(
      List(3, 4, 5, 4, 3),
      List(4, 0, 0, 0, 4),
      List(5, 0, 0, 0, 5),
      List(4, 0, 0, 0, 4),
      List(3, 4, 5, 4, 3)
    ))

    val step2 = Day11.Board(List(
      List(4, 5, 6, 5, 4),
      List(5, 1, 1, 1, 5),
      List(6, 1, 1, 1, 6),
      List(5, 1, 1, 1, 5),
      List(4, 5, 6, 5, 4)
    ))

    assertEquals((9, step1), start.step)
    assertEquals((0, step2), step1.step)

  @Test def part2(): Unit =
    assertEquals(195, Day11.part2(input))
