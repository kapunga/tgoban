package org.kapunga.tgoban

import scala.reflect.ClassTag

/**
 * A MetaBoard is a base type for intersectional data on a Go board.  The data
 * could be anything from a board position of an actual game, to a field mapping
 * of black or white influence on the board.  This class provides basic access to
 * points on the board and some simple methods for accessing neighbors, etc.
 *
 * @param size The size of a board on the side, for example 19 in the case
 *             of a standard 19 x 19 board.
 * @param emptyValue The value to initialize each point on the board with, for
 *                   example and empty space in the case of a MetaBoard representing
 *                   the game positions.
 * @tparam T The type of data stored on this MetaBoard
 * Created by kapunga on 5/30/14.
 */
class MetaBoard[T:ClassTag](size: Int, emptyValue: T) {
  // Board data, pre-filled with a default value
  val boardData: Array[Array[T]] = Array.fill[T](size, size) { emptyValue }
  // A simple filter to determine if a position is on the board.
  val isOnBoard = (pos: (Int, Int)) => pos._1 >= 0 && pos._1 < size && pos._2 >= 0 && pos._2 < size

  /**
   * @return The size of this board per side.
   */
  def getSize(): Int = { size }

  /**
   * Used for accessing the data stored at a given position on the MetaBoard
   * @param pos A tuple specifying a coordinate on the board.
   * @return The value of data on the board or the emptyValue in the constructor
   *         if the provided coordinates are not on the board.
   */
  def getPointValue(pos: (Int, Int)): T = {
    if (isOnBoard(pos)) {
      return boardData(pos._1)(pos._2)
    } else {
      return emptyValue
    }
  }

  /**
   * Used to provide a Set of neighboring points to a given board position.
   * @param pos The position on the board we are checking.
   * @return The neighboring intersections on the board, or an empty set if
   *         the provided coordinates are not on the board.
   */
  def getNeighbors(pos: (Int, Int)): Set[(Int, Int)] = {
    // If you aren't on the board, you don't have any neighbors.
    if (!isOnBoard(pos)) {
      return Set()
    }
    // Default list of neighbors, filter out those that are not on the board.
    val neighbors: Set[(Int, Int)] = Set((pos._1 + 1, pos._2),
                                         (pos._1 - 1, pos._2),
                                         (pos._1, pos._2 + 1),
                                         (pos._1, pos._2 - 1));
    neighbors.filter(isOnBoard)
  }

  /**
   * Returns a Set of neighboring points to a given position that meet a certain
   * criteria.
   * @param pos The position on the board we are checking.
   * @param validPoint A function mapping data we find at an intersection to
   *                   a Boolean specifying whether we want the coordinate returned.
   * @return
   */
  def getQualifiedNeighbors(pos: (Int, Int), validPoint: (T => Boolean)): Set[(Int, Int)] = {
    getNeighbors(pos).filter((neighbor: (Int, Int)) => validPoint(getPointValue(neighbor)))
  }

  /**
   * Used to get a String representation of the data on this MetaBoard for human
   * viewing.
   * @param mapper A mapping function that takes point data from the board and maps it
   *               to a string.
   * @return A board shaped (hopefully, depending on the mapping function) string that
   *         represents the data on this board.
   */
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
