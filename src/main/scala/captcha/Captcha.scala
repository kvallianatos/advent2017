package captcha

import scala.io.Source

object Captcha extends App {

  def readInput(path: String): String = {
    /*for (line <- Source.fromInputStream(getClass.getResourceAsStream("/captcha_input.txt")).getLines) {
      println(line)
    }*/
    Source.fromInputStream(getClass.getResourceAsStream("/captcha_input.txt")).getLines.drop(5).next
  }

  val input1 = readInput("captcha_input.txt").toList.map(_.asDigit)
  println(input1)
  val result1 = input1.foldLeft((input1.last,0)) { (acc, value) =>
    if (acc._1 == value) (value, acc._2 + value)
    else (value, acc._2)
  }
  println(s"Take 1: ${result1._2}")

  val input2 = readInput("captcha_input.txt").toArray.map(_.asDigit)
  val result2 = (0 to input2.length - 1).foldLeft((input2, 0)) { (acc, value) =>
    val scanIndex = (value + acc._1.length/2) % acc._1.length
    if (acc._1(value) == acc._1(scanIndex)) (acc._1, acc._2 + acc._1(value))
    else (acc._1, acc._2)
  }
  println(s"RESULT: ${result2._2}")
}
