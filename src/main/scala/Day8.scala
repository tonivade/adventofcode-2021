import scala.io.Source

object Day8:

  def part1(input: List[String]): Int =
    val uniqueDigits = Set(2, 3, 4, 7)
    val all = input.flatMap(_.split("\\|")(1).trim.split(" "))
    all.filter(x => uniqueDigits.contains(x.size)).size

  /*
   * 1. 2 segments
   * 4. 4 segments
   * 7. 3 segments
   * 8. 7 segments
   * 9. is the only with 6 segments that contains all 4s segments
   * 3. is the only with 5 segments that contains all 1s segments
   * 5. between 2 and 5, if 9s segments removed from 5 then no segments left, so this one is 5 and the other is 2
   * 2. see 5
   * 0. between 0 and 6, if 1s segments removed from 0 then only 4 segmets left, so this one is 0 and the other one is 6
   * 6. see 0
   */
  def part2(input: List[String]): Int = 
    val left = input.map(_.split("\\|")(0).trim.split(" ").toList)
    val right = input.map(_.split("\\|")(1).trim.split(" ").toList)
    val all = left zip right

    all.map(translate).sum
  
  def translate(dictionary: List[String], input: List[String]): Int = 
    val map = translation(dictionary)
    input.map(_.sorted).map(map).mkString.toInt

  def translation(input: List[String]): Map[String, Int] =

    val grouped = input.groupBy(_.length)
    
    val one = grouped(2)(0)
    val seven = grouped(3)(0)
    val four = grouped(4)(0)
    val eight = grouped(7)(0)

    val (nine, zeroOrSix) = parse9(grouped(6), four)
    val (three, twoOrFive) = parse3(grouped(5), one)
    val (two, five) = parse2or5(twoOrFive, nine)
    val (zero, six) = parse0or6(zeroOrSix, one)

    Map(
      one.sorted -> 1, 
      two.sorted -> 2, 
      three.sorted -> 3, 
      four.sorted -> 4, 
      five.sorted -> 5,
      six.sorted -> 6,
      seven.sorted -> 7,
      eight.sorted -> 8,
      nine.sorted -> 9, 
      zero.sorted -> 0)

  def parse9(input6: List[String], four: String): (String, List[String]) = 
    val nine = input6.find(x => four.forall(x.contains)).get
    val zeroOrSix = input6.filter(_ != nine)
    (nine, zeroOrSix)

  def parse3(input5: List[String], one: String): (String, List[String]) = 
    val three = input5.find(x => one.forall(x.contains)).get
    val twoOrFive = input5.filter(_ != three)
    (three, twoOrFive)

  def parse2or5(twoOrFive: List[String], nine: String): (String, String) =
    val five = twoOrFive.find(_.filterNot(nine.contains).isEmpty).get
    (twoOrFive.find(_ != five).get, five)
  
  def parse0or6(zeroOrSix: List[String], one: String): (String, String) =
    val zero = zeroOrSix.find(_.filterNot(one.contains).size == 4).get
    (zero, zeroOrSix.find(_ != zero).get)

@main def main8: Unit = 
  val input = Source.fromFile("input/day8.txt").getLines.toList

  println(s"Day8 part1: ${Day8.part1(input)}")
  println(s"Day8 part2: ${Day8.part2(input)}")