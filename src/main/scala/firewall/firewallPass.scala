package firewall

import scala.io.Source

object firewallPass extends App {

  def checkSentry(step: Int, depth: Int): Boolean = {
    var position = step % ((2 * depth) - 2)
    if (position >= depth) position = depth - (position - depth + 2)
    position == 0
  }

  def calculateLayerSeverity(range: Int, depth: Int): Int = {
    range * depth
  }

  def calculateTotalSeverity(path: List[(Int,Int)], delay: Int): Int = {
    path
      .filter(x => checkSentry(x._1 + delay, x._2))
      .map(x => calculateLayerSeverity(x._1, x._2))
      .sum
  }

  def checkIfPathClear(path: List[(Int,Int)], delay: Int): Boolean = {
    path.filter(x => checkSentry(x._1 + delay, x._2)).size == 0
  }

  def findDelay(path: List[(Int,Int)]): Int = {
    var i = 0
    while(!checkIfPathClear(path, i)) i += 1
    i
  }

  val path = Source.fromInputStream(getClass.getResourceAsStream("/firewall.txt")).getLines.toList
    .map(input => input.split(": ").take(2))
    .map(layer => (layer(0).toInt, layer(1).toInt))

  println(calculateTotalSeverity(path, 0))
  println(findDelay(path))

}
