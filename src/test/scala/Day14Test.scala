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
    val mapping = Map(('N', 'N') -> 'C', ('N', 'C') -> 'B', ('C', 'B') -> 'H')
    assertEquals("NCNBCHB", Day14.step("NNCB", mapping))

  @Test def step4(): Unit = 
    val (seed, mapping) = Day14.parse(input)
    assertEquals("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB", Day14.go(4, seed, mapping))

  @Test def part2(): Unit = 
    assertEquals(BigInt(2188189693529L), Day14.part2(input))

  @Test def step1_part2(): Unit = 
    val mapping = Map(('N', 'N') -> 'C', ('N', 'C') -> 'B', ('C', 'B') -> 'H')
    assertEquals(Day14.pairs("NCNBCHB"), Day14.step2(Day14.pairs("NNCB"), mapping))

  @Test def step4_part2(): Unit = 
    val (seed, mapping) = Day14.parse(input)
    assertEquals(Day14.pairs("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"), Day14.go2(4, Day14.pairs("NNCB"), mapping))