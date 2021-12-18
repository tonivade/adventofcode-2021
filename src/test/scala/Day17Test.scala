import org.junit.Test
import org.junit.Assert.*

class Day17Test:
  import Day17.{Target, Probe, launch }

  val input: String = "target area: x=20..30, y=-10..-5"

  val initial = (0, 0)
  val target = Target(20, 30, -10, -5)

  @Test def target1(): Unit =
    assertEquals(
      Some(Probe((28,-7), (0,-5), List((27,-3), (25,0), (22,2), (18,3), (13,3), (7,2), (0,0)))), 
      launch(target)(Probe(initial, (7, 2))))

  @Test def target2(): Unit =
    assertEquals(
      Some(Probe((21,-9), (0,-6), List((21,-4), (21,0), (21,3), (20,5), (18,6), (15,6), (11,5), (6,3), (0,0)))), 
      launch(target)(Probe(initial, (6, 3))))

  @Test def target3(): Unit =
    assertEquals(
      Some(Probe((30,-6), (5,-4), List((24,-3), (17,-1), (9,0), (0,0)))), 
      launch(target)(Probe(initial, (9, 0))))

  @Test def target4(): Unit =
    val result = launch(target)(Probe(initial, (6, 9)))
    assertEquals(
      Some(Probe((21,-10), (0,-11), List((21,0), (21,9), (21,17), (21,24), (21,30), (21,35), (21,39), (21,42), (21,44), (21,45), (21,45), (21,44), (21,42), (21,39), (20,35), (18,30), (15,24), (11,17), (6,9), (0,0)))), 
      result)
    assertEquals(45, result.get.maxY)

  @Test def target5(): Unit =
    assertEquals(
      None, 
      launch(target)(Probe(initial, (17, -4))))

  @Test def part1(): Unit = 
    assertEquals(45, Day17.part1(input))

  @Test def part2(): Unit = 
    assertEquals(112, Day17.part2(input))
