package org.kapunga.tgoban

import org.kapunga.tgoban.BoardPoint.BoardPoint
import org.kapunga.tgoban.BoardPoint.EMPTY
import org.kapunga.tgoban.BoardPoint.BLACK
import org.kapunga.tgoban.BoardPoint.WHITE
import org.kapunga.tgoban.BoardSize.BoardSize

/**
 * A concrete representation of a game board.  The intersections represent three states,
 * occupied by a black stone, a white stone, or unoccupied.  Has method for searching for
 * basic things, such as counting liberties contained by a set of stones, finding connected
 * stones or connected empty space, etc.
 * 
 * @param size The size of a board on the side, for example 19 in the case
 *             of a standard 19 x 19 board.
 * @author Paul J Thordarson kapunga@gmail.com
 */
class GameBoard(size: Int) extends MetaBoard[BoardPoint](size, EMPTY) {
  /**
   * Used to get the list of points adjacent to a give point that are occupied
   * by a stone of a given color (or no color)
   * @param pnt The point we are looking for adjacent stones from.
   * @param color The color of adjacent stones we are looking to match.
   * @return A set of points adjacent to pnt of the given color
   */
  def getAdjacentStones(pnt: Banten, color: BoardPoint): Set[Banten] = {
    getQualifiedNeighbors(pnt, (point: BoardPoint) => point == color)
  }

  /**
   * Gets a Set of connected points adjacent to the Set of test points.  All of the test points must be the same
   * color or all be empty and the resultant set will match the state of the test points.  Used to find strongly
   * connected groups of stones or liberties.
   *
   * @param testPoints The set of test points to check.
   * @return A set of points connected to the test points, or an empty set if the set of test points is either
   *         empty or inconsistent.
   */
  def getConnectedStones(testPoints: Set[Banten]): Set[Banten] = {
    if (testPoints.size == 0) return Set()

    val color: BoardPoint = getPointValue(testPoints.head)

    getConnectedPoints(testPoints, (point: BoardPoint) => point == color)
  }

  /**
   * Counts the total number of liberties a group of stones has.  Does NOT look for more stones in the group
   * @param stones The set of stones to count liberties for.
   * @return The total number of unique liberties shared by the set of stones, or 0 if the set of
   *         stones is empty, inconsistent, or made up of empty spaces.
   */
  def countLiberties(stones: Set[Banten]): Int = {
    // TODO Maybe make this just check one stone and find the connected groups?
    if (!stones.forall((pnt: Banten) => getPointValue(pnt) != EMPTY
                                        && getPointValue(pnt) == getPointValue(stones.head))) {
      return 0
    }

    var emptySpaces: Set[Banten] = Set()

    for (stone <- stones) emptySpaces = emptySpaces ++ getAdjacentStones(stone, EMPTY)

    emptySpaces.size
  }

  /**
   * Used to determine if placing a given stone on an intersection would result in a legal
   * state NOT accounting for ko.  Placing empty spaces is always valid.  Placing a stone
   * on top of another stone is not valid.
   * @param pnt The point we are checking the validity for.
   * @param color The color we are checking the validity for.
   * @return true if the move would be valid, otherwise false.
   */
  def isLegalMove(pnt: Banten, color: BoardPoint) : Boolean = {
    // It's not a legal move if it's not on the board.
    if (!isOnBoard(pnt)) return false

    // If we are trying to make a point on the board blank, it's fine.
    if (color == EMPTY) return true

    // If the space we are trying to play on is not vacant, it is not a legal move.
    if (getPointValue(pnt) != EMPTY) return false

    // If the space we are moving to has at least one liberty, it's legal
    if (getQualifiedNeighbors(pnt, (point: BoardPoint) => point == EMPTY).size > 0) return true

    // If the space we are moving to has no liberties, but the adjacent friendly stones have one, it's legal
    if (countLiberties(getConnectedStones(getAdjacentStones(pnt, color))) > 1) return true

    // If the space we are moving to has no liberties, but the opposing pieces don't have any either, it's legal
    if (countLiberties(getConnectedStones(getAdjacentStones(pnt, color.opp))) == 1) return true

    // If it hasn't been deemed legal by now, it is not.
    false
  }

  /**
   * Places a (or removes) a stone on the board representation and returns a list of prisoners.
   * This occurs only if the move is valid.
   * @param pnt The point where we are placing the stone.
   * @param color The color of the stone we are placing.
   * @return A tuple representing the number and type of prisoners removed from the baord if any.
   */
  def placeStone(pnt: Banten, color: BoardPoint): Agehama = {
    def getCaptures(pnt: Banten): Agehama = {
      var capturedStones: Set[Banten] = Set()

      val testSet: Set[Banten] = getAdjacentStones(pnt, getPointValue(pnt).opp)

      testSet.foreach((f: Banten) => {
        val connected: Set[Banten] = getConnectedStones(Set(f))

        if (countLiberties(getConnectedStones(connected)) == 0) capturedStones = capturedStones ++ connected
      })

      capturedStones.foreach((p: Banten) => boardData(p.x)(p.y) = EMPTY)

      (capturedStones.size, getPointValue(pnt).opp)
    }

    if (isLegalMove(pnt, color)) {
      boardData(pnt.x)(pnt.y) = color
      return getCaptures(pnt)
    }

    (0, EMPTY)
  }

  /**
   * Used for visualizing the game board.
   * @return A string representation of the current game state.
   */
  def gamePosition(): String = {
    this.getBoardRepr((pnt: BoardPoint) => pnt.repr)
  }
}

object GameBoard {
  val handicapStones: List[Banten] = List((3, 3), (15, 15), (3, 15), (15, 3), (3, 9), (15, 9), (9, 3), (15, 3), (9, 9))
  val primeHandicapStones: List[Banten] = List((3, 3), (15, 15), (3, 15), (15, 3), (9, 9), (15, 9), (9, 3))

  def apply(size: BoardSize, handicap: Int): GameBoard = {
    val sizeNum: Int = size match {
        case BoardSize.STANDARD => 19
        case BoardSize.MEDIUM => 13
        case BoardSize.SMALL => 9
    }

    val board = new GameBoard(sizeNum)

    board
  }
}