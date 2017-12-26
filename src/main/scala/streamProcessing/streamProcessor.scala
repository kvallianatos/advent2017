package streamProcessing

import scala.collection.mutable
import scala.io.Source

object streamProcessor extends App {

  val inputIter: Iterator[Char] = Source.fromInputStream(getClass.getResourceAsStream("/block_stream.txt")).getLines.mkString.iterator
  val blockStack = mutable.Stack[String]()
  var score: Int = 0
  var garbageChars: Int = 0

  def removeGarbage(i: Iterator[Char]): Int = {
    while(inputIter.hasNext) {
      inputIter.next() match {
        case '!' => inputIter.next()
        case '>' => return 1
        case _ => garbageChars += 1
      }
    }
    return 0
  }

  while(inputIter.hasNext) {
    inputIter.next() match {
      case '{' => blockStack.push("{")
      case '}' => score += blockStack.size; blockStack.pop()
      case '<' => removeGarbage(inputIter)
      case _ =>
    }
  }
  println(s"Score: $score")
  println(s"Garbage chars: $garbageChars")
}
