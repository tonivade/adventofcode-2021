import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day16Test:
  import Day16.{ Literal, Operator, parse }

  val input = ""

  @Test def parse1(): Unit =
    assertEquals(Literal(6, 2021), parse("D2FE28"))

  @Test def parse2(): Unit =
    assertEquals(Operator(1, List(Literal(6, 10), Literal(2, 20))), parse("38006F45291200"))

  @Test def part10(): Unit = 
    assertEquals(9, Day16.part1("38006F45291200"))

  @Test def part11(): Unit = 
    assertEquals(16, Day16.part1("8A004A801A8002F478"))

  @Test def part12(): Unit = 
    assertEquals(12, Day16.part1("620080001611562C8802118E34"))

  @Test def part13(): Unit = 
    assertEquals(23, Day16.part1("C0015000016115A2E0802F182340"))

  @Test def part14(): Unit = 
    assertEquals(31, Day16.part1("A0016C880162017C3686B18A3D4780"))

  @Test @Ignore def part2(): Unit = 
    assertEquals(0, Day16.part2(input))