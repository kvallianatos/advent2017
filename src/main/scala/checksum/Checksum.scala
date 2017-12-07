package checksum

import scala.io.Source

object Checksum extends App {

  val input: Seq[Array[Int]] = Source.fromInputStream(getClass.getResourceAsStream("/checksum_input.txt"))
                                    .getLines.toList.map(_.split("\\s+")
                                    .map(s => s.trim.toInt))

  def lineDiff(numbers: Array[Int]): Int = {
    numbers.max - numbers.min
  }

  def lineMod(div: Int, input: Array[Int]): Int = {
    val x = input.headOption.getOrElse(return div)
    input.tail.foreach(y => if (x % y == 0) return x/y)
    lineMod(div, input.tail)
  }

  def generateChecksumPart1(acc: Int, input: Seq[Array[Int]]): Int = {
    generateChecksumPart1(acc + lineDiff(input.headOption.getOrElse(return acc)), input.tail)
  }
  println(generateChecksumPart1(0, input))

  def generateChecksumPart2(acc: Int, input: Seq[Array[Int]]): Int = {
    generateChecksumPart2(acc + lineMod(0, input.headOption.getOrElse(return acc).sorted(Ordering.Int.reverse)), input.tail)
  }
  println(generateChecksumPart2(0, input))
}
