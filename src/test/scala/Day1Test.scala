import org.junit.Test
import org.junit.Assert.*

class Day1Test:

  val list = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  @Test def t1(): Unit = 
    assertEquals(7, Day1.solve(list))
