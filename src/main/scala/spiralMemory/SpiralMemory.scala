package spiralMemory

object SpiralMemory extends App {

  def modValue(position: Int, edgeSize: Int, height: Int): Int = {
    ((position - Math.pow((edgeSize-2),2)) % (height * 2)).toInt
  }

  def calculateSteps(position: Int): Int = {
    val edgeSize = Stream.from(1, 2).dropWhile(x => Math.pow(x,2) < position).take(1).head
    val height = (edgeSize-1)/2
    height + Math.abs(modValue(position, edgeSize, height) - (height))
  }

  println(calculateSteps(289326))
}
