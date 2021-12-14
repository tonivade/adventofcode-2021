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
    assertEquals(1588, Day14.part1(input))

  @Test def step1(): Unit = 
    val mapping = Map("NN" -> "C", "NC" -> "B", "CB" -> "H")
    assertEquals("NCNBCHB", Day14.step("NNCB", mapping))

  @Test def step4(): Unit = 
    val (seed, mapping) = Day14.parse(input)
    assertEquals("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB", Day14.go(4, seed, mapping))

  @Test def part2(): Unit = 
    assertEquals(0, Day14.part2(input))