import org.junit.Test
import org.junit.Assert.*

class Day14Test:

  val input = """NNCB
                |
                |CH -> B
                |HH -> N
                |CB -> H
                |NH -> C
                |HB -> C
                |HC -> B
                |HN -> C
                |NN -> C
                |BH -> H
                |NC -> B
                |NB -> B
                |BN -> B
                |BB -> N
                |BC -> B
                |CC -> N
                |CN -> C""".stripMargin

  @Test def part1(): Unit = 
    assertEquals(0, Day14.part1(input))

  @Test def part2(): Unit = 
    assertEquals(0, Day14.part2(input))