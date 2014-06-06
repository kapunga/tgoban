package org.kapunga.tgoban.sgf

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

  def movePart = moveProp | placeProp | intProp | floatProp | stringProp

  def moveParts = movePart*

  def move: Parser[SgfNode] = ";" ~> moveParts ^^ buildNode

  def branch: Parser[List[SgfNode]] = "(" ~> rep(move | branch) <~ ")" ^^ buildBranch

  def game: Parser[SgfNode] =  "(" ~> move ~ rep(move | branch) <~ ")" ^^ { case head ~ body => buildGame(head, body) }

  def parseCoord: (String => (Int, Int)) = (coord: String) => {
    val offset = 'a'.asInstanceOf[Int]
    (coord.charAt(1).asInstanceOf[Int] - offset,
      coord.charAt(0).asInstanceOf[Int] - offset)
  }

  def buildNode: ((List[(String, Any)]) => SgfNode) = (propList: List[(String, Any)]) => {
    val node: SgfNode = new SgfNode()
    propList.foreach((item: (String, Any)) => node.propMap = node.propMap + (item._1 -> item._2))
    node
  }

  def buildBranch: (List[Object] => List[SgfNode]) = (nodeList: List[Object]) => {
    var head: SgfNode = new SgfNode()
    var tail: SgfNode = head

    nodeList.foreach((node) => {
      if (node.isInstanceOf[SgfNode]) {
        tail.addChild(node.asInstanceOf[SgfNode])
        tail = node.asInstanceOf[SgfNode]
      } else if (node.isInstanceOf[List[SgfNode]]) {
        node.asInstanceOf[List[SgfNode]].foreach((item: SgfNode) => tail.addChild(item))
      }
    })

    head.children
  }

  def buildGame: ((SgfNode, List[Object]) => SgfNode) = (head: SgfNode, list: List[Object]) => {
    head.children = buildBranch(list)

    head
  }
}