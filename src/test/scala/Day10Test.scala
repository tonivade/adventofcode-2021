import org.junit.Test
import org.junit.Assert.*

class Day10Test:

  val input = """[({(<(())[]>[[{[]{<()<>>
                |[(()[<>])]({[<{<<[]>>(
                |{([(<{}[<>[]}>{[]{[(<()>
                |(((({<>}<{<{<>}{[]{[]{}
                |[[<[([]))<([[{}[[()]]]
                |[{[{({}]{}}([{[{{{}}([]
                |{<[[]]>}<{[{[{[]{()[[[]
                |[<(<(<(<{}))><([]([]()
                |<{([([[(<>()){}]>(<<{{
                |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\n").toList

  @Test def part1(): Unit = 
    assertEquals(26397, Day10.part1(input))

  @Test def part2(): Unit =
    assertEquals(288957L, Day10.part2(input))
