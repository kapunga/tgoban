package org.kapunga.tgoban

import org.junit.{Before, Test}
import org.junit.Assert._

/**
 * @author Paul J Thordarson kapunga@gmail.com
 */
class MetaBoardTests {
  var metaBoard: MetaBoard[Int] = new MetaBoard[Int](5, -5)

  var qualBoard:MetaBoard[Int] = new MetaBoard[Int](6, 0)

  def testEven(pntVal: Int): Boolean = { pntVal % 2 == 0 }

  def testQualification(lim: Int)(pntVal: Int): Boolean = { pntVal <= lim + 1 && pntVal >= lim - 1 }

  @Before
  def setUpTestBoards() = {
    metaBoard = new MetaBoard[Int](5, -5)
    qualBoard = new MetaBoard[Int](6, 0)

    for (i <- 0 to qualBoard.getSize - 1) {
      for (j <- 0 to qualBoard.getSize - 1) {
        qualBoard.boardData(i)(j) = i + j
      }
    }
  }

  @Test
  def testConstructor() = {
    assertEquals("The size was incorrectly constructed.", 5, metaBoard.getSize)
    assertEquals("The board was incorrectly filled.", -5, metaBoard.getPointValue(2, 2))
  }

  @Test
  def testOffBoard() = {
    assertFalse("isOnBoard not correctly identifying both coordinates off.", metaBoard.isOnBoard((-1, -1)))
    assertFalse("isOnBoard not correctly identifying both coordinates off.", metaBoard.isOnBoard((-1, 5)))
    assertFalse("isOnBoard not correctly identifying both coordinates off.", metaBoard.isOnBoard((5, -1)))
    assertFalse("isOnBoard not correctly identifying both coordinates off.", metaBoard.isOnBoard((5, 5)))

    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((0, -1)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((-1, 0)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((4, -1)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((0, 5)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((-1, 4)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((5, 0)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((5, 4)))
    assertFalse("isOnBoard not correctly identifying one coordinate off.", metaBoard.isOnBoard((4, 5)))
  }

  @Test
  def testOnBoard() = {
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((0, 0)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((4, 0)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((0, 4)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((4, 4)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((0, 2)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((2, 0)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((4, 2)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((2, 4)))
    assertTrue("isOnBoard not correctly identifying both coordinates on.", metaBoard.isOnBoard((2, 2)))
  }

  @Test
  def testGetNeighborsOffBoard() = {
    assertEquals("Off board point is returning neighbors", metaBoard.getNeighbors((-1, -1)), Set())
  }

  @Test
  def testGetNeighborsCorner() = {
    assertEquals("Neighbor count in the corner is off.", metaBoard.getNeighbors((0, 0)), Set((0, 1), (1, 0)))
    assertEquals("Neighbor count in the corner is off.", metaBoard.getNeighbors((4, 0)), Set((4, 1), (3, 0)))
    assertEquals("Neighbor count in the corner is off.", metaBoard.getNeighbors((0, 4)), Set((0, 3), (1, 4)))
    assertEquals("Neighbor count in the corner is off.", metaBoard.getNeighbors((4, 4)), Set((4, 3), (3, 4)))
  }

  @Test
  def testGetNeighborsSide() = {
    assertEquals("Neighbor count on the side is off.", metaBoard.getNeighbors((0, 2)), Set((0, 1), (0, 3), (1, 2)))
    assertEquals("Neighbor count on the side is off.", metaBoard.getNeighbors((2, 0)), Set((1, 0), (3, 0), (2, 1)))
    assertEquals("Neighbor count on the side is off.", metaBoard.getNeighbors((4, 2)), Set((4, 1), (4, 3), (3, 2)))
    assertEquals("Neighbor count on the side is off.", metaBoard.getNeighbors((2, 4)), Set((1, 4), (3, 4), (2, 3)))
  }

  @Test
  def testGetNeighborsCenter() = {
    assertEquals("Neighbor count in center is off.", metaBoard.getNeighbors((2, 2)),
                 Set((2, 3), (3, 2), (1, 2), (2, 1)))
  }

  @Test
  def testGetQualifiedNeighbors() = {
    assertEquals("Qualified neighbor count is off.", qualBoard.getQualifiedNeighbors((0, 0), testEven),
                 Set())
    assertEquals("Qualified neighbor count is off.", qualBoard.getQualifiedNeighbors((0, 5), testEven),
                 Set((1, 5), (0, 4)))

    assertEquals("Qualified neighbor count is off.", qualBoard.getQualifiedNeighbors((0, 2), testEven),
                 Set())
    assertEquals("Qualified neighbor count is off.", qualBoard.getQualifiedNeighbors((0, 3), testEven),
                 Set((0, 2), (0, 4), (1, 3)))

    assertEquals("Qualified neighbor count is off.", qualBoard.getQualifiedNeighbors((2, 2), testEven),
                 Set())
    assertEquals("Qualified neighbor count is off.", qualBoard.getQualifiedNeighbors((2, 3), testEven),
                 Set((1, 3), (3, 3), (2, 2), (2, 4)))
  }

  @Test
  def testBadConnectedPointsData() = {
    assertEquals("Connected points with bad data is returning values", qualBoard.getConnectedPoints(Set((3, 3)),
                 testQualification(0)), Set())
  }

  @Test
  def testGetConnectedPoints() = {
    assertEquals("Connected count is off.", qualBoard.getConnectedPoints(Set((0, 0)), testQualification(0)),
                 Set((0, 0), (1, 0), (0, 1)))

    assertEquals("Connected count is off.", qualBoard.getConnectedPoints(Set((3, 3)), testQualification(5)),
                 Set((0, 4), (0, 5), (1, 3), (1, 4),
                     (1, 5), (2, 2), (2, 3), (2, 4),
                     (3, 1), (3, 2), (3, 3), (4, 0),
                     (4, 1), (4, 2), (5, 0), (5, 1)))
  }
}
