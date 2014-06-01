package org.kapunga.tgoban

/**
 * A list of alias types for tGoban.
 * Created by kapunga on 6/1/14.
 */
object TgobanTypes {
  /**
   * Represents a point on a go board.  From the Japanese word "Ban" for board and "ten" for point. (盤点)
   * Point is an overloaded term, so that was avoided.
   */
  type Banten = Tuple2[Int, Int]
}
