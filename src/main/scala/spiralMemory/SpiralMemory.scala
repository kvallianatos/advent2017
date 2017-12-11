package spiralMemory

object SpiralMemory extends App {

  def edgeModValue(position: Int, edgeSize: Int, height: Int): Int = {
    ((position - Math.pow((edgeSize-2),2)) % (height * 2)).toInt
  }

  def calculateSteps(position: Int): Int = {
    val edgeSize = Stream.from(1, 2).dropWhile(x => Math.pow(x,2) < position).take(1).head
    val height = (edgeSize-1)/2
    height + Math.abs(edgeModValue(position, edgeSize, height) - (height))
  }

  println(calculateSteps(289326))

  def edgePosToAbsPos(height: Int, edge: Int, modValue: Int): Int = {
      if (height == 0) 0
      else if (modValue - height == 1) (height * 2 * edge) + modValue -1 + Math.pow(((height - 1) * 2 + 1),2).toInt
      else (height * 2 * edge) + modValue -1 + Math.pow(((height - 1) * 2 + 1),2).toInt
  }

  def getNextValue(limit: Int, values: Array[Int], height: Int, position: Int): Int = {
    def sumPreviousValues(indexes: List[Int]): Int = {
      indexes.filter(_ >= 0).map(i => values(i)).sum
    }

    val edgeSize: Int = height * 2 + 1
    val edgeNumber: Int = ((position) - (Math.pow(edgeSize-2,2).toInt)) / (height * 8 / 4)

    val result = sumPreviousValues(List(position - 2)) + sumPreviousValues(
      (edgeModValue(position, edgeSize, height) match {
        case 0 if (edgeNumber == 4) =>                  List(edgePosToAbsPos(height - 1, edgeNumber, 0), edgePosToAbsPos(height, 0, 1))
        case 0 =>                                       List(edgePosToAbsPos(height - 1, edgeNumber, 0))
        case 1 if (edgeSize == 3 && edgeNumber == 3) => List((position - 3), edgePosToAbsPos(height - 1, edgeNumber, 0), edgePosToAbsPos(height, 0, 1))
        case 1 if (edgeNumber == 0) =>                  List(edgePosToAbsPos(height - 1, edgeNumber, 1))
        case 1 if (height == 1) =>                      List((position - 3), edgePosToAbsPos(height - 1, edgeNumber, 0))
        case 1 =>                                       List((position - 3), edgePosToAbsPos(height - 1, edgeNumber, 0), edgePosToAbsPos(height - 1, edgeNumber, 1))
        case 2 if (edgeNumber == 0) =>                  List((position - 3), edgePosToAbsPos(height - 1, edgeNumber, 1), edgePosToAbsPos(height - 1, edgeNumber, 2))
        case n if (n == edgeSize - 2 && edgeNumber != 3) => List(edgePosToAbsPos(height - 1, edgeNumber, n-1), edgePosToAbsPos(height - 1, edgeNumber, n-2))
        case n => {
          val mid = edgePosToAbsPos(height - 1, edgeNumber, n-1)
          ((mid - 1 to mid + 1).toList)
        }
      }))

    val nextHeight = if (edgeNumber == 4) height + 1 else height

    if (result > limit) return result
    else getNextValue(limit, values :+ result, nextHeight, position+1)
  }

  println(getNextValue(289326, Array(1,1), 1, 3))
}
