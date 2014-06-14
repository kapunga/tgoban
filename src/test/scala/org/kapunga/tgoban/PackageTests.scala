package org.kapunga.tgoban

import org.junit.Test
import org.junit.Assert._
import org.kapunga.tgoban.BoardPoint._

/**
 * @author Paul J Thordarson kapunga@gmail.com
 */
class PackageTests {
  @Test
  def testBantenCoords() = {
    val banten: Banten = (1, 3)

    assertEquals("Banten coordinate lookup is failing for x.", 1, banten.x)
    assertEquals("Banten coordinate lookup is failing for x.", 3, banten.y)
  }

  @Test
  def testBoardPointOpposite() = {
    val black: BoardPoint = BLACK
    val white: BoardPoint = WHITE
    val empty: BoardPoint = EMPTY

    assertEquals("BLACK opposite is incorrect", WHITE, black.opp)
    assertEquals("WHITE opposite is incorrect", BLACK, white.opp)
    assertEquals("EMPTY opposite is incorrect", EMPTY, empty.opp)
  }
}
