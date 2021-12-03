import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day3Test:

  val list = List(
    "00100", 
    "11110", 
    "10110", 
    "10111", 
    "10101", 
    "01111", 
    "00111", 
    "11100", 
    "10000", 
    "11001", 
    "00010", 
    "01010")

  @Test def part1(): Unit = 
    assertEquals(198, Day3.part1(list))

  @Test @Ignore def part2(): Unit = ???


