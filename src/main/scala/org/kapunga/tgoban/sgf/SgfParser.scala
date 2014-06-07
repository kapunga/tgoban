package org.kapunga.tgoban.sgf

import scala.io.Source
import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

/**
 *
 * @author Paul J Thordarson kapunga@gmail.com.
 */
class SgfParser extends JavaTokenParsers {
  // A list of property tokens that denote a move by black or white.
  def moveProps: String = "(B|W)"

  def boardCoord = "[" ~> "[a-zA-Z]{2}".r <~ "]" ^^ parseCoord
  def moveProp: Parser[(String, (Int, Int))] = moveProps.r ~ boardCoord ^^ { case key ~ value => (key, value) }

  // A list of property tokens that denote placement of multiple stones.
  def placeProps: String = "(AB|AW)"

  def coordSet = boardCoord+
  def placeProp: Parser[(String, List[(Int, Int)])] = placeProps.r ~ coordSet ^^ { case key ~ value => (key, value) }

  // A list that denotes a numerical property such as the file format
  // version number or the number of handicap stones.
  def intProps: String = "(FF|GM|SZ|HA|TM)"

  def intVal = "[" ~> wholeNumber <~ "]"
  def intProp: Parser[(String, Int)] = intProps.r ~ intVal ^^ { case key ~ value =>
    (key, value.toInt) }

  // A list that denotes a floating point property like komi
  def floatProps: String = "(KM)"

  def floatVal = "[" ~> floatingPointNumber <~ "]"
  def floatProp: Parser[(String, Float)] = floatProps.r ~ floatVal ^^ { case key ~ value =>
    (key, value.toFloat) }

  // A list of property tokens that denote a string property such as a
  // comment or a player's name.
  def stringProps: String = "(R[U,E]|PB|PW|EV|GN|C[A]?|DT|SO|B[C,R]|W[C,R])"

  def stringVal = "[" ~> "[^(\\[\\])]+".r <~ "]"
  def stringProp: Parser[(String, String)] = stringProps.r ~ stringVal ^^ { case key ~ value => (key, value) }

  def moveProperty = moveProp | placeProp | intProp | floatProp | stringProp

  /**
   * A move property list parser
   */
  def moveProperties = moveProperty*

  /**
   * A move parser
   * move ::== ";" { moveProperty }
   */
  def move: Parser[SgfNode] = ";" ~> moveProperties ^^ buildNode

  /**
   * A branch parser
   * branch ::== "(" { move | branch } ")"
   */

  def branch: Parser[List[SgfNode]] = "(" ~> rep(move | branch) <~ ")" ^^ buildBranch

  /**
   * A game parser
   * game ::== "(" move { move | branch } ")"
   */
  def game: Parser[SgfNode] =  "(" ~> move ~ rep(move | branch) <~ ")" ^^ { case head ~ body => buildGame(head, body) }

  /**
   * Builds a coordinate pair from a pair of characters. These characters are reversed so that the visual
   * matches correctly.
   *
   * @return A coordinate pair.
   */
  def parseCoord: (String => (Int, Int)) = (coord: String) => {
    val offset = 'a'.asInstanceOf[Int]
    (coord.charAt(1).asInstanceOf[Int] - offset,
      coord.charAt(0).asInstanceOf[Int] - offset)
  }

  /**
   * Builds a node.
   * @return The node being built.
   */
  def buildNode: ((List[(String, Any)]) => SgfNode) = (propList: List[(String, Any)]) => {
    val node: SgfNode = new SgfNode()
    propList.foreach((item: (String, Any)) => node.propMap = node.propMap + (item._1 -> item._2))
    node
  }

  /**
   * Builds a branch.
   * @return The head node of the branch.
   */
  def buildBranch: (List[Object] => List[SgfNode]) = (nodeList: List[Object]) => {
    var head: SgfNode = new SgfNode()
    var tail: SgfNode = head

    nodeList.foreach((node) => {
      node match {
        case child: SgfNode =>
          tail.addChild(child)
          tail = child
        case nodes: List[SgfNode] =>
          nodes.foreach((item: SgfNode) => tail.addChild(item))
        case _ =>
      }
    })

    head.children
  }

  /**
   * Builds a game.
   *
   * @return The head node of the game.
   */
  def buildGame: ((SgfNode, List[Object]) => SgfNode) = (head: SgfNode, list: List[Object]) => {
    head.children = buildBranch(list)

    head
  }
}

object SgfParser {
  def parseSgfFile(fileName: String): SgfNode = {
    val parser: SgfParser = new SgfParser()

    val result = parser.parseAll(parser.game, Source.fromFile(fileName).reader)

    if (result.isEmpty) return new SgfNode()
    else return result.get
  }
}