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

  @Test def part10(): Unit = 
    assertEquals(Day18.parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"), 
                 Day18.addAll("""[[[[4,3],4],4],[7,[[8,4],9]]]
                                |[1,1]""".stripMargin.split("\n").toList))

  @Test def part11(): Unit = 
    assertEquals(Day18.parse("[[[[1,1],[2,2]],[3,3]],[4,4]]"), 
                 Day18.addAll("""[1,1]
                                |[2,2]
                                |[3,3]
                                |[4,4]""".stripMargin.split("\n").toList))

  @Test def part12(): Unit = 
    assertEquals(Day18.parse("[[[[3,0],[5,3]],[4,4]],[5,5]]"), 
                 Day18.addAll("""[1,1]
                                |[2,2]
                                |[3,3]
                                |[4,4]
                                |[5,5]""".stripMargin.split("\n").toList))

  @Test def part13(): Unit = 
    assertEquals(Day18.parse("[[[[5,0],[7,4]],[5,5]],[6,6]]"), 
                 Day18.addAll("""[1,1]
                                |[2,2]
                                |[3,3]
                                |[4,4]
                                |[5,5]
                                |[6,6]""".stripMargin.split("\n").toList))

  @Test def part14(): Unit = 
    assertEquals(Day18.parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"), 
                 Day18.addAll("""[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                                |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                                |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                                |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                                |[7,[5,[[3,8],[1,4]]]]
                                |[[2,[2,2]],[8,[8,1]]]
                                |[2,9]
                                |[1,[[[9,3],9],[[9,0],[0,7]]]]
                                |[[[5,[7,4]],7],1]
                                |[[[[4,2],2],6],[8,7]]""".stripMargin.split("\n").toList))

  @Test @Ignore def part2(): Unit = 
    assertEquals(0, Day18.part2(Nil))
