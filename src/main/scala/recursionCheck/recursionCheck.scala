package recursionCheck

import scala.io.Source

object recursionCheck extends App {

  val processRegex1 = raw"(\w+) \((\d+)\) -> (.+)".r
  val processRegex2 = raw"(\w+) \((\d+)\)".r

  val parents = (for (
    inputLine <- Source.fromInputStream(getClass.getResourceAsStream("/process_tree.txt")).getLines();
    m1 <- processRegex1.findAllIn(inputLine).matchData)
  yield (m1.group(1), m1.group(3).split(",\\s").toList)).toMap

  val processes = (for (
    inputLine <- Source.fromInputStream(getClass.getResourceAsStream("/process_tree.txt")).getLines();
    m2 <- processRegex2.findAllIn(inputLine).matchData)
    yield (m2.group(1), m2.group(2).toInt)).toMap

  val rootProcess = parents.keySet.filter(processName => !parents.values.toSet.flatten.contains(processName)).mkString(" ")
  println(s"Root process is: $rootProcess")

  def correctWeight(processesWithSummedWeights: Seq[(String, Int)], weights: Map[String, Int]) = {
    val groupedByWeight = processesWithSummedWeights.groupBy(_._2)
    val incorrectProcess = groupedByWeight.filter(p => p._2.size == 1).head
    val correctWeight = groupedByWeight.filter(p => p._2.size != 1).head._1
    val adjustAmount = incorrectProcess._1 - correctWeight
    println(s"Change weight of ${incorrectProcess._2.head._1} to ${weights(incorrectProcess._2.head._1) - adjustAmount}")
  }

  def sumWeights(processName: String, weights: Map[String, Int], processTree: Map[String, List[String]]): Int = {
    if (!processTree.keySet.contains(processName)) weights(processName)
    else {
      val subweights: Seq[(String, Int)] = processTree(processName).map(name => (name,sumWeights(name, weights, processTree)))
      val totalWeight = weights(processName) + subweights.map(x => x._2).sum
      if (subweights.map(x => x._2).toSet.size > 1) {
        correctWeight(subweights, weights)
      }
      totalWeight
    }
  }

  val totalProcessWeight = sumWeights(rootProcess, processes, parents)
  println(s"Total process weight is: $totalProcessWeight")
}
