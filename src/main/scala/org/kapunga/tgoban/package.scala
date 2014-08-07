package org.kapunga

import org.kapunga.tgoban.BoardPoint.BoardPoint

/**
 * @author Paul J Thordarson kapunga@gmail.com
 */
package object tgoban {
  /**
   * A type alias for Tuple2(Int, Int) since coordinates are usually passed around in pairs.
   */
  type Banten = (Int, Int)

  /**
   * A class used to extend Banten so that you can use the more intuitive 'x' and 'y' instead of
   * _1 and _2 which are not as intuitive.
   *
   * @param banten A type aliased Tuple2(Int, Int)
   */
  class BantenCoords(banten: Banten) {
    def x: Int = { banten._1 } //
    def y: Int = { banten._2 }
  }

  /**
   * Implicitly wraps a Banten type alias in a class that allows you to intuitively access the members.
   *
   * @param banten The banten we are extending.
   * @return The wrapper class BantenCoords
   */
  implicit def bantenToCoords(banten: Banten): BantenCoords = new BantenCoords(banten)

  object BoardSize extends Enumeration {
    type BoardSize = Value
    val STANDARD, MEDIUM, SMALL = Value
  }

  /**
   * An enumeration that represents
   */
  object BoardPoint extends Enumeration {
    type BoardPoint = Value
    val EMPTY, BLACK, WHITE = Value
  }

  /**
   * Wraps
   * @param pnt The BoardPoint we want to extend.
   */
  class BoardPointRepr(pnt: BoardPoint) {
    /**
     *
     * @return
     */
    def repr: String = {
      pnt match {
        case BoardPoint.EMPTY => ". "
        case BoardPoint.BLACK => "* "
        case BoardPoint.WHITE => "o "
      }
    }

    /**
     *
     * @return
     */
    def opp: BoardPoint = {
      pnt match {
        case BoardPoint.EMPTY => BoardPoint.EMPTY
        case BoardPoint.BLACK => BoardPoint.WHITE
        case BoardPoint.WHITE => BoardPoint.BLACK
      }
    }
  }

  /**
   *
   * @param pnt
   * @return
   */
  implicit def boardPointRepr(pnt: BoardPoint): BoardPointRepr = { new BoardPointRepr(pnt) }

  /**
   *
   */
  type Agehama = (Int, BoardPoint)
}
