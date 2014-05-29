package org.kapunga.tgoban

import org.kapunga.tgoban.BoardPoint.BoardPoint
import org.kapunga.tgoban.BoardPoint.EMPTY
import org.kapunga.tgoban.BoardPoint.BLACK
import org.kapunga.tgoban.BoardPoint.WHITE

/**
 * Created by kapunga on 5/28/14.
 */
class GameBoard(size: Int) extends MetaBoard[BoardPoint](size, EMPTY) {
  def isLegalMove(pos: (Int, Int), color: BoardPoint) : Boolean = { true }

  def getCapturesAfter(pos: (Int, Int)) : (Int, BoardPoint) = { (0, EMPTY) }

  def placeStone(pos: (Int, Int), color: BoardPoint) : (Int, BoardPoint) = {
    if (isLegalMove(pos, color)) {
      boardData(pos._1)(pos._2) = color
    }
    return (0, EMPTY)
  }

  def getAdjacentStones(pos: (Int, Int), color: BoardPoint) : List[(Int, Int)] = {
    getQualifiedNeighbors(pos, (point: BoardPoint) => point == color)
  }

  def boardState(): String = {
    this.getBoardRepr(BoardPoint.getIntersectionString)
  }
}

object BoardPoint extends Enumeration {
  type BoardPoint = Value
  val EMPTY, BLACK, WHITE = Value

  def getIntersectionString(point: BoardPoint) : String = {
    if (point == EMPTY) {
      return ". "
    } else if (point == BLACK) {
      return "* "
    } else {
      return "o "
    }
  }
}