import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day18Test:
  import Day18.{ Pair, Leaf }

  val input: String = """"""

  @Test def parse(): Unit = 
    assertEquals(Pair(Leaf(1), Leaf(2)), Day18.parse("[1,2]"))
    assertEquals(Pair(Pair(Leaf(1), Leaf(2)), Leaf(3)), Day18.parse("[[1,2],3]"))
  
  @Test def add(): Unit =
    assertEquals(Pair(Pair(Leaf(1), Leaf(2)), Pair(Leaf(3), Leaf(4))), Pair(Leaf(1), Leaf(2)) add Pair(Leaf(3), Leaf(4)))

  @Test def split(): Unit =
    assertEquals(Pair(Pair(Leaf(5), Leaf(5)), Leaf(1)), Day18.reduce(Pair(Leaf(10), Leaf(1)))) 
    assertEquals(Pair(Pair(Leaf(5), Leaf(6)), Leaf(1)), Day18.reduce(Pair(Leaf(11), Leaf(1)))) 

  @Test def explode(): Unit =
    assertEquals(
      Pair(Pair(Pair(Pair(Leaf(0),Leaf(9)),Leaf(2)),Leaf(3)),Leaf(4)), 
      Day18.reduce(Pair(Pair(Pair(Pair(Pair(Leaf(9),Leaf(8)),Leaf(1)),Leaf(2)),Leaf(3)),Leaf(4)))) 

  @Test def reduce(): Unit = 
    val a = Day18.parse("[[[[4,3],4],4],[7,[[8,4],9]]]")
    val b = Day18.parse("[1,1]")
    val expected = Day18.parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    assertEquals(expected, Day18.reduce(a add b))

  @Test @Ignore def part1(): Unit = 
    assertEquals(0, Day18.part1(input))

  @Test @Ignore def part2(): Unit = 
    assertEquals(0, Day18.part2(input))
