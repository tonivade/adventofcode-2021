import scala.io.Source

object Day8:

  def part1(input: List[String]): Int =
    val uniqueDigits = Set(2, 3, 4, 7)
    val all = input.flatMap(_.split("\\|")(1).trim.split(" "))
    all.filter(x => uniqueDigits.contains(x.size)).size

  /*
   * acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
   * 
   *           0. ??????
   *  dddd     1. ab            x1. a
   * e    a    2. ?????         x2. b
   * e    a    3. ?????         x3. d
   *  ffff     4. eafb          x4. e
   * g    b    5. ?????         x5. f
   * g    b    6. ??????        x6. g
   *  cccc     7. dab           x7. c
   *           8. acedgfb
   *           9. ??????
   * 
   * 2 -> 1
   * 3 -> 7
   * 4 -> 4
   * 5 -> 2 3 5
   * 6 -> 0 6 9
   * 7 -> 8
   */
  def part2(input: List[String]): Int = 
    val left = input.map(_.split("\\|")(0).trim.split(" ").toList)
    val right = input.map(_.split("\\|")(1).trim.split(" ").toList)
    val all = left zip right

    all.map {
      (dictionary, values) => translate(values, translation(dictionary))
    }.sum
  
  def translate(output: List[String], translate: Map[String, Int]): Int = 
    output.map(_.sorted).map(translate).mkString.toInt

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

    Map.empty 
      + (one.sorted -> 1) 
      + (two.sorted -> 2) 
      + (three.sorted -> 3) 
      + (four.sorted -> 4) 
      + (five.sorted -> 5)
      + (six.sorted -> 6)
      + (seven.sorted -> 7)
      + (eight.sorted -> 8)
      + (nine.sorted -> 9) 
      + (zero.sorted -> 0)

  def parse9(input6: List[String], four: String): (String, List[String]) = 
    val nine = input6.find(x => four.forall(x.contains(_))).get
    val zeroOrSix = input6.filter(_ != nine)
    (nine, zeroOrSix)

  def parse3(input5: List[String], one: String): (String, List[String]) = 
    val three = input5.find(x => one.forall(x.contains(_))).get
    val twoOrFive = input5.filter(_ != three)
    (three, twoOrFive)

  def parse2or5(twoOrFive: List[String], nine: String): (String, String) =
    val five = twoOrFive.find(x => x.filterNot(nine.contains).isEmpty).get
    (twoOrFive.find(_ != five).get, five)
  
  def parse0or6(zeroOrSix: List[String], one: String): (String, String) =
    val zero = zeroOrSix.find(x => x.filterNot(one.contains).size == 4).get
    (zero, zeroOrSix.find(_ != zero).get)

@main def main8: Unit = 
  val input = Source.fromFile("input/day8.txt").getLines.toList

  println(s"Day8 part1: ${Day8.part1(input)}")
  println(s"Day8 part2: ${Day8.part2(input)}")