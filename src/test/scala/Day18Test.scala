import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day18Test:
  import Day18.{ Pair, Leaf }

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
      Pair(Pair(Pair(Pair(Leaf(0), Leaf(9)), Leaf(2)), Leaf(3)), Leaf(4)), 
      Day18.reduce(Pair(Pair(Pair(Pair(Pair(Leaf(9), Leaf(8)), Leaf(1)), Leaf(2)), Leaf(3)), Leaf(4)))) 

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

  @Test def part15(): Unit = 
    assertEquals(Day18.parse("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"), 
                 Day18.addAll("""[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                                |[[[5,[2,8]],4],[5,[[9,9],0]]]
                                |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                                |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                                |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                                |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                                |[[[[5,4],[7,7]],8],[[8,3],8]]
                                |[[9,3],[[9,9],[6,[4,9]]]]
                                |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                                |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.split("\n").toList))

  @Test def magnitude(): Unit =
    assertEquals(143, Day18.part1(List("[[1,2],[[3,4],5]]")))
    assertEquals(1384, Day18.part1(List("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))

  @Test def part1(): Unit =
    assertEquals(4140, 
                 Day18.part1("""[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                               |[[[5,[2,8]],4],[5,[[9,9],0]]]
                               |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                               |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                               |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                               |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                               |[[[[5,4],[7,7]],8],[[8,3],8]]
                               |[[9,3],[[9,9],[6,[4,9]]]]
                               |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                               |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.split("\n").toList))

  @Test def part2(): Unit = 
    assertEquals(3993, 
                 Day18.part2("""[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                               |[[[5,[2,8]],4],[5,[[9,9],0]]]
                               |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                               |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                               |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                               |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                               |[[[[5,4],[7,7]],8],[[8,3],8]]
                               |[[9,3],[[9,9],[6,[4,9]]]]
                               |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                               |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.split("\n").toList))
