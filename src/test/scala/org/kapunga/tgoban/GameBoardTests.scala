package org.kapunga.tgoban

import org.kapunga.tgoban.BoardPoint._

/**
 * Created by kapunga on 6/27/14.
 */
class GameBoardTests {
  /**
   * Gets a full size test board.
   *
   * @return A 19x19 board with a few moves on it.
   */
  def getTestBoard: GameBoard = {
    val gameBoard = new GameBoard(19)

    gameBoard.placeStone((2, 3), BLACK)
    gameBoard.placeStone((15, 3), WHITE)
    gameBoard.placeStone((15, 15), BLACK)
    gameBoard.placeStone((3, 16), WHITE)
    gameBoard.placeStone((3, 14), BLACK)
    gameBoard.placeStone((2, 14), WHITE)
    gameBoard.placeStone((2, 13), BLACK)
    gameBoard.placeStone((2, 15), WHITE)
    gameBoard.placeStone((3, 13), BLACK)
    gameBoard.placeStone((5, 16), WHITE)
    gameBoard.placeStone((3, 9), BLACK)

    gameBoard
  }

  /**
   * Gets a 9x9 test board.
   * @return A 9x9 board with a few moves on it.
   */
  def getSmallTestBoard: GameBoard = {
    val gameBoard = new GameBoard(9)

    gameBoard.placeStone((0, 1), WHITE)
    gameBoard.placeStone((1, 0), WHITE)
    gameBoard.placeStone((1, 1), WHITE)
    gameBoard.placeStone((0, 2), BLACK)
    gameBoard.placeStone((1, 2), BLACK)
    gameBoard.placeStone((2, 0), BLACK)
    gameBoard.placeStone((2, 1), BLACK)
    gameBoard.placeStone((2, 3), BLACK)

    gameBoard
  }
}
