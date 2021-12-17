import scala.io.Source
import scala.annotation.tailrec

object Day16:

  def binaryToLong(binary: String): Long = 
    binary.reverse.zipWithIndex.filter((x, _) => x == '1').map((_, i) => 1L << i).sum

  def binaryToInt(binary: String): Int = 
    binary.reverse.zipWithIndex.filter((x, _) => x == '1').map((_, i) => 1 << i).sum

  sealed trait Packet

  case class Literal(version: Int, value: Long) extends Packet

  case class Operator(version: Int, operation: Int, operands: List[Packet]) extends Packet

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
        (Literal(version, binaryToLong(value)), consumed)
      // operator
      case _ => 
        binary.drop(3 + 3).charAt(0) match {
          // bits
          case '0' => 
            val bitsLength = binaryToInt(binary.drop(3 + 3 + 1).take(15))
            val bits = binary.drop(3 + 3 + 1 + 15).take(bitsLength)
            val (packets, consumed) = parseBits(bits, bitsLength)
            (Operator(version, typeId, packets), 3 + 3 + 1 + 15 + consumed)
          // packets
          case '1' => 
            val packetLength = binaryToInt(binary.drop(3 + 3 + 1).take(11))
            val bits = binary.drop(3 + 3 + 1 + 11)
            val (packets, consumed) = parsePackets(bits, packetLength)
            (Operator(version, typeId, packets), 3 + 3 + 1 + 11 + consumed)
        }
    }

  def parsePackets(binary: String, size: Int): (List[Packet], Int) = 
    if (size == 0)
      (Nil, 0)
    else
      val (head, consumed) = parseOne(binary)
      val (tail, consumeAll) = parsePackets(binary.drop(consumed), size - 1)
      (head :: tail, consumed + consumeAll)

  def parseBits(binary: String, size: Int): (List[Packet], Int) = 
    if (binary.length < 6)
      (Nil, 0)
    else
      val (head, consumed) = parseOne(binary)
      val (tail, consumedAll) = parseBits(binary.drop(consumed), size - consumed)
      (head :: tail, consumed + consumedAll)

  def part1(input: String): Int =
    val packet = parse(input)
    def count(packet: Packet): Int =
      packet match {
        case Literal(version, value) => version
        case Operator(version, _, packets) => version + packets.map(count).sum
      }
    count(packet)

  def part2(input: String): Long = 
    val packet = parse(input)
    def eval(packet: Packet): Long =
      packet match {
        case Literal(_, value) => value
        case Operator(_, 0, packets) => packets.map(eval).foldLeft(0L)(_ + _)
        case Operator(_, 1, packets) => packets.map(eval).foldLeft(1L)(_ * _)
        case Operator(_, 2, packets) => packets.map(eval).min
        case Operator(_, 3, packets) => packets.map(eval).max
        case Operator(_, 5, List(a, b)) => if (eval(a) > eval(b)) 1L else 0L
        case Operator(_, 6, List(a, b)) => if (eval(a) < eval(b)) 1L else 0L
        case Operator(_, 7, List(a, b)) => if (eval(a) == eval(b)) 1L else 0L
        case _ => throw new java.lang.IllegalArgumentException()
      }
    eval(packet)

@main def main16: Unit = 
  val input = Source.fromFile("input/day16.txt").getLines.next

  println(s"Day16 part1: ${Day16.part1(input)}")
  println(s"Day16 part2: ${Day16.part2(input)}")
  