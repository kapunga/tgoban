package org.kapunga.tgoban

/**
 * A list of alias types for tGoban.
 * @author Paul J Thordarson kapunga@gmail.com.
 */
object TgobanTypes {
  /**
   * Represents a point on a go board.  From the Japanese word "Ban" for board and "ten" for point. (盤点)
   * Point is an overloaded term, so that was avoided.
   */
  type Banten = (Int, Int)
}
