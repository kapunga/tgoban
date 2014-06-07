package org.kapunga.tgoban.sgf

import org.kapunga.tgoban.sgf.SgfProperties.SgfProperty

/**
 *
 * @author Paul J Thordarson kapunga@gmail.com
 */
class SgfNode(p: SgfNode = null) {
  var parent = p
  var children: List[SgfNode] = List()
  // val propMap: Map[SgfProperty, String] = Map()
  var propMap: Map[String, Any] = Map()

  if (parent != null) parent.addChild(this)

  def SgfNode(size: Int) {
    propMap = propMap + ("FF" -> 4)
    propMap = propMap + ("GM" -> 1)
    propMap = propMap + ("SZ" -> size)
  }

  def addChild(child: SgfNode): Unit = {
    children = child :: children
    child.parent = this
  }


  def setParent(p:SgfNode): Unit = { parent = p }

  def getParent: SgfNode = { parent }
}
