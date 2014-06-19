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
 *
 * @author Paul J Thordarson kapunga@gmail.com
 */
class MetaBoard[T:ClassTag](size: Int, emptyValue: T) {
  // Board data, pre-filled with a default value
  val boardData: Array[Array[T]] = Array.fill[T](size, size) { emptyValue }
  // A simple filter to determine if a position is on the board.
  val isOnBoard = (pnt: Banten) => pnt.x >= 0 && pnt.x < size && pnt.y >= 0 && pnt.y < size

  /**
   * Gets the size of the Go board.  Go boards are square, so a size of 19 corresponds
   * to a 19x19 board.
   *
   * @return The size of this board per side.
   */
  def getSize: Int = { size }

  /**
   * Used for accessing the data stored at a given position on the MetaBoard.
   * @param pnt A tuple specifying a coordinate on the board.
   * @return The value of data on the board or the emptyValue in the constructor
   *         if the provided coordinates are not on the board.
   */
  def getPointValue(pnt: Banten): T = {
    if (isOnBoard(pnt)) {
      boardData(pnt.x)(pnt.y)
    } else {
      emptyValue
    }
  }

  /**
   * Used to provide a Set of neighboring points to a given board position.
   * @param pnt The position on the board we are checking.
   * @return The neighboring intersections on the board, or an empty set if
   *         the provided coordinates are not on the board.
   */
  def getNeighbors(pnt: Banten): Set[Banten] = {
    // If you aren't on the board, you don't have any neighbors.
    if (!isOnBoard(pnt)) {
      return Set()
    }
    // Default list of neighbors, filter out those that are not on the board.
    val neighbors: Set[Banten] = Set((pnt.x + 1, pnt.y),
                                    (pnt.x - 1, pnt.y),
                                    (pnt.x, pnt.y + 1),
                                    (pnt.x, pnt.y - 1))
    neighbors.filter(isOnBoard)
  }

  /**
   * Returns a Set of neighboring points to a given position that meet a certain
   * criteria.
   * @param pnt The position on the board we are checking.
   * @param validPoint A function mapping data we find at an intersection to
   *                   a Boolean specifying whether we want the coordinate returned.
   * @return The Set of neighboring points that pass the validPoint function.
   */
  def getQualifiedNeighbors(pnt: Banten, validPoint: (T => Boolean)): Set[Banten] = {
    getNeighbors(pnt).filter((neighbor: Banten) => validPoint(getPointValue(neighbor)))
  }

  /**
   * Gets a Set of points strongly connected (via adjacent spaces) to a Set of test points
   * provided that match a certain test.  An example would be two Black points are handed to
   * this method, and a set of stones is returned matching all of the stones in groups of which
   * those two stones are a member.  This method first checks that the passed set of
   * points in internally consistent with the test, then uses tail recursion to get the total set.
   * This method will return an empty set if the input set of points do not all match the passed
   * function for determining which points belong.
   *
   * @param testPoints The set of test points to find connected stones to.
   * @param valid A function for determining of a stone belongs in the list of connected stones.
   * @return The inclusive set of points connected to "testPoints", or the empty set if "testPoints"
   *         is an invalid set according to the "valid" function.
   */
  def getConnectedPoints(testPoints: Set[Banten], valid: (T => Boolean)): Set[Banten] = {
    if (!testPoints.forall((p: Banten) => valid(getPointValue(p)))) return Set()

    def recurseConnectedPoints(visited: Set[Banten], connected: Set[Banten], valid: (T => Boolean)): Set[Banten] = {
      if (connected.diff(visited).size == 0) return connected

      val nextPoint = connected.diff(visited).head

      recurseConnectedPoints(visited + nextPoint, connected ++ getQualifiedNeighbors(nextPoint, valid), valid)
    }
    
    recurseConnectedPoints(Set(), testPoints, valid)
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
        boardString += mapper(getPointValue (i, j))
      }

      boardString += "\n"
    }

    boardString
  }
}
