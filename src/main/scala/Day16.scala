import scala.io.Source
import scala.annotation.tailrec

object Day16:

  def binaryToInt(binary: String): Int = 
    binary.reverse.zipWithIndex.filter((x, _) => x == '1').map((_, i) => 1 << i).sum

  sealed trait Packet:
    def version: Int

  case class Literal(version: Int, value: Int) extends Packet

  case class Operator(version: Int, operands: List[Packet]) extends Packet

  val translation: Map[Char, String] = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  def parse(input: String): Packet =
    val binary = input.flatMap(translation)
    val (packet, _) = parseOne(binary)
    packet

  def parseOne(binary: String): (Packet, Int) = 
    val version = binaryToInt(binary.take(3))
    val typeId = binaryToInt(binary.drop(3).take(3))

    typeId match {
      // literal
      case 4 => 
        def go(input: String): String =
          val current = input.take(5)
          if (current.charAt(0) == '1')
            current.drop(1) + go(input.drop(5))
          else
            current.drop(1)

        val value = go(binary.drop(3 + 3))
        val consumed = 3 + 3 + value.size + (value.size / 4)
        (Literal(version, binaryToInt(value)), consumed)
      // operator
      case _ => 
        binary.drop(3 + 3).charAt(0) match {
          // bits
          case '0' => 
            val bitLength = binaryToInt(binary.drop(3 + 3 + 1).take(15))
            val bits = binary.drop(3 + 3 + 1 + 15).take(bitLength)
            val (packets, consumed) = parseMany(bits)
            assert(bitLength == consumed)
            (Operator(version, packets), 3 + 3 + 1 + 15 + bitLength)
          // packets
          case '1' => 
            val packetLength = binaryToInt(binary.drop(3 + 3 + 1).take(11))
            val bits = binary.drop(3 + 3 + 1 + 11)
            val (packets, consumed) = parseMany(bits, packetLength)
            assert(packetLength == packets.size)
            (Operator(version, packets), 3 + 3 + 1 + 11 + consumed)
        }
    }

  def parseMany(binary: String, size: Int): (List[Packet], Int) = 
    if (size == 0)
      (Nil, 0)
    else
      val (head, consumed) = parseOne(binary)
      val (tail, consumeAll) = parseMany(binary.drop(consumed), size - 1)
      (head :: tail, consumed + consumeAll)

  def parseMany(binary: String): (List[Packet], Int) = 
    if (binary.length < 6)
      (Nil, 0)
    else
      val (head, consumed) = parseOne(binary)
      val (tail, consumedAll) = parseMany(binary.drop(consumed))
      (head :: tail, consumed + consumedAll)

  def part1(input: String): Int =
    val packet = parse(input)
    def count(packet: Packet): Int =
      packet match {
        case Literal(version, value) => version
        case Operator(version, packets) => version + packets.map(count).sum
      }
    count(packet)

  def part2(input: String): Int = ???

@main def main16: Unit = 
  val input = Source.fromFile("input/day16.txt").getLines.next

  println(s"Day16 part1: ${Day16.part1(input)}")
  println(s"Day16 part2: ${Day16.part2(input)}")
  