import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day16Test:
  import Day16.{ Literal, Operator, parse }

  @Test def parse1(): Unit =
    assertEquals(Literal(6, 2021), parse("D2FE28"))

  @Test def parse2(): Unit =
    assertEquals(Operator(1, 6, List(Literal(6, 10), Literal(2, 20))), parse("38006F45291200"))

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

  @Test def part20(): Unit = 
    assertEquals(3, Day16.part2("C200B40A82"))

  @Test def part21(): Unit = 
    assertEquals(54, Day16.part2("04005AC33890"))

  @Test def part22(): Unit = 
    assertEquals(7, Day16.part2("880086C3E88112"))

  @Test def part23(): Unit = 
    assertEquals(9, Day16.part2("CE00C43D881120"))

  @Test def part24(): Unit = 
    assertEquals(1, Day16.part2("D8005AC2A8F0"))

  @Test def part25(): Unit = 
    assertEquals(0, Day16.part2("F600BC2D8F"))

  @Test def part26(): Unit = 
    assertEquals(0, Day16.part2("9C005AC2F8F0"))

  @Test def part27(): Unit = 
    assertEquals(1, Day16.part2("9C0141080250320F1802104A08"))