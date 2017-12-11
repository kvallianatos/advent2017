package cpuJumps

import scala.io.Source

object CpuJumps extends App {

  def inc(i: Int): Int = i + 1
  def incSpecial(i: Int): Int = if (i >= 3) i-1 else i+1

  def processJump(instr: Array[Int], counter: Int, pointer: Int, f: Int => Int): Int = {
    if (pointer >= instr.length) counter
    else {
      val steps = instr(pointer)
      instr(pointer) = f(steps)
      processJump(instr, counter + 1, steps + pointer, f)
    }
  }

  val jumps = Source.fromInputStream(getClass.getResourceAsStream("/jump_instructions.txt")).getLines.map(_.toInt).toArray
  println(s"Part one: ${processJump(jumps, 0,0, inc)}")

  val jumps2 = Source.fromInputStream(getClass.getResourceAsStream("/jump_instructions.txt")).getLines.map(_.toInt).toArray
  println(s"Part one: ${processJump(jumps2, 0,0, incSpecial)}")
}
