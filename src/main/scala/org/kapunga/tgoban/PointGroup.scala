package org.kapunga.tgoban

/**
 * A trait indicating some arbitrary group of points on a go board.
 * It is up to the implementing classes to determine what exactly they
 * could be.  It could be a tight group of connected stones for instance.
 * A group should be associated with a board, otherwise the coordinates 
 * don't necessarily make any sense.
 *
 * @author Paul J Thordarson kapunga@gmail.com
 */
trait PointGroup {
  /**
   * Used for getting the members of the group, a Set of points on a board
   *
   * @return The member points in this group.
   */
  def getMembers: Set[(Int, Int)]

  /**
   * Used for accessing the board this group refers to.
   *
   * @return The MetaBoard this group is associated with.
   */
  def getBoard: MetaBoard[_]

  /**
   * This method should be implemented to check the group for
   * correctness.
   *
   * @return true if this group is valid, otherwise false.
   */
  def validate: Boolean
}
