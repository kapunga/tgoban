package org.kapunga.tgoban


/**
 * Created by kapunga on 6/7/14.
 */
class Game(size: Int, handicap: Int, komi: Float) {
  val board = new GameBoard(size)

}

abstract class Move
case class Handicap(stones: Set[Banten]) extends Move
case class White() extends Move
case class Black() extends Move


