package memoryAllocation

import scala.io.Source

object MemoryAllocation extends App {

  val memory = Source.fromInputStream(getClass.getResourceAsStream("/memory_alloc.txt")).getLines.next.toString.split("\\s+").toList.map(x => x.toInt)
  val allocationLog = scala.collection.mutable.Set[String](memory.mkString)

  def findLargestBank(memBanks: List[Int]): (Int,Int) = {
    memBanks.zipWithIndex.maxBy(_._1)
  }

  def reallocateMemory(memBanks: List[Int]): List[Int] = {
    
    def distributeBlocks(memBanks: List[Int], index: Int, blockCount: Int): List[Int] = {
      if (blockCount == 0) memBanks
      else distributeBlocks(memBanks.updated(index, memBanks(index) + 1),
        if (index + 1 == memBanks.size) 0 else index + 1,
        blockCount - 1)
    }

    val (blockCount, index) = findLargestBank(memBanks)
    val newAllocation = distributeBlocks(memBanks.updated(index, 0), if (index + 1 == memBanks.size) 0 else index + 1, blockCount)
    if (allocationLog.add(newAllocation.mkString)) reallocateMemory(newAllocation)
    else newAllocation
  }

  val memoryLoopPosition = reallocateMemory(memory)
  println(s"Part one: ${allocationLog.size}")

  allocationLog.clear()
  allocationLog.add(memoryLoopPosition.mkString)
  reallocateMemory(memoryLoopPosition)
  println(s"Part one: ${allocationLog.size}")
}
