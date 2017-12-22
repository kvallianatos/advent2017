package registerInstructions

import scala.io.Source

object registerUpdate extends App {

  val registers = scala.collection.mutable.Map[String, Int]()
  var value = 0

  for (line <- Source.fromInputStream(getClass.getResourceAsStream("/register_instructions.txt")).getLines) {
    println(line)

    val in: Array[String] = line.split("\\s+")
    if (condition(in(5))(read(in(4)), in(6).toInt)) upd(in(0), regOp(in(1))(read(in(0)), in(2).toInt))
  }

  println(s"Part 1: ${registers.maxBy(_._2)}")
  println(s"Part 2: $value")

  def regOp(operation: String): (Int, Int) => Int = {
    operation match {
      case "inc" => (a, b) => a + b
      case "dec" => (a, b) => a - b
    }
  }

  def condition(comparator: String): (Int, Int) => Boolean = {
    comparator match {
      case ">" => (a, b) => a > b
      case "<" => (a, b) => a < b
      case ">=" => (a, b) => a >= b
      case "<=" => (a, b) => a <= b
      case "==" => (a, b) => a == b
      case "!=" => (a, b) => a != b
    }

  }

  def upd(reg: String, incrementValue: Int) = {
    //println(s"setting register $reg to $incrementValue")
    value = Math.max(value, incrementValue)
    registers(reg) = incrementValue
  }

  def read(reg: String): Int = {
    if (!registers.contains(reg)) registers(reg) = 0
    registers(reg)
  }

}
