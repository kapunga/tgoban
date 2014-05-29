package org.kapunga.tgoban

import scala.reflect.ClassTag

/**
 *
 * @param size
 * @param emptyValue
 * Created by kapunga on 5/30/14.
 */
abstract class MetaBoard[T:ClassTag](size: Int, emptyValue: T) {
  // Board data, pre-filled with a default value
  val boardData: Array[Array[T]] = Array.fill[T](size, size) { emptyValue }
  // A simple filter to determine if a position is on the board.
  val isOnBoard = (pos: (Int, Int)) => pos._1 >= 0 && pos._1 < size && pos._2 >= 0 && pos._2 < size

  /**
   * Returns the size of this board.
   * @return
   */
  def getSize(): Int = { size }

  /**
   *
   * @param pos
   * @return
   */
  def getPointValue(pos: (Int, Int)): T = {
    if (isOnBoard(pos)) {
      return boardData(pos._1)(pos._2)
    } else {
      return emptyValue
    }
  }

  /**
   *
   * @param pos
   * @return
   */
  def getNeighbors(pos: (Int, Int)): List[(Int, Int)] = {
    // If you aren't on the board, you don't have any neighbors.
    if (!isOnBoard(pos)) {
      return List()
    }
    // Default list of neighbors, filter out those that are not on the board.
    val neighbors: List[(Int, Int)] = List((pos._1 + 1, pos._2),
                                           (pos._1 - 1, pos._2),
                                           (pos._1, pos._2 + 1),
                                           (pos._1, pos._2 - 1));
    neighbors.filter(isOnBoard)
  }

  /**
   *
   * @param pos
   * @param validPoint
   * @return
   */
  def getQualifiedNeighbors(pos: (Int, Int), validPoint: (T => Boolean)) = {
    getNeighbors(pos).filter((neighbor: (Int, Int)) => validPoint(getPointValue(neighbor)))
  }

  def getBoardRepr(mapper: (T => String)): String = {
    var boardString = ""

    for (i <- 0 to size - 1) {
      for (j <- 0 to size - 1) {
        boardString += mapper(getPointValue(i, j))
      }

      boardString += "\n"
    }

    return boardString
  }
}
