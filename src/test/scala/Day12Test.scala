import org.junit.Test
import org.junit.Assert.*
import org.junit.Ignore

class Day12Test:

  val inputS = """start-A
                 |start-b
                 |A-c
                 |A-b
                 |b-d
                 |A-end
                 |b-end""".stripMargin.split("\n").toList

  val inputM = """dc-end
                 |HN-start
                 |start-kj
                 |dc-start
                 |dc-HN
                 |LN-dc
                 |HN-end
                 |kj-sa
                 |kj-HN
                 |kj-dc""".stripMargin.split("\n").toList

  val inputL = """fs-end
                 |he-DX
                 |fs-he
                 |start-DX
                 |pj-DX
                 |end-zg
                 |zg-sl
                 |zg-pj
                 |pj-he
                 |RW-he
                 |fs-DX
                 |pj-RW
                 |zg-RW
                 |start-pj
                 |he-WI
                 |zg-he
                 |pj-fs
                 |start-RW""".stripMargin.split("\n").toList

  @Test def part1S(): Unit = 
    assertEquals(10, Day12.part1(inputS))

  @Test @Ignore def part1M(): Unit = 
    assertEquals(19, Day12.part1(inputM))

  @Test @Ignore def part1L(): Unit = 
    assertEquals(226, Day12.part1(inputL))

  @Test @Ignore def part2(): Unit =
    assertEquals(0, Day12.part2(List.empty))
