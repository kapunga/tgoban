package org.kapunga.tgoban.engine

import org.kapunga.tgoban.GameBoard

/**
 * Created by kapunga on 8/14/14.
 */
trait MoveCalculator {
  def update(board: GameBoard)
  def enabled(): Boolean
  def poll(): OpinionMap
}
