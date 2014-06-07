package org.kapunga.tgoban.sgf

/**
 *
 * @author Paul J Thordarson kapunga@gmail.com
 */
object SgfProperties extends Enumeration {
  val FF = new SgfProperty("FF", "FileFormat")
  val GM = new SgfProperty("GM", "Game")
  val SZ = new SgfProperty("SZ", "Size")
  val CA = new SgfProperty("CA", "Character set")
  val GN = new SgfProperty("GN", "Game Name")
  val EV = new SgfProperty("EV", "Event")
  val PB = new SgfProperty("PB", "Player Black")
  val PW = new SgfProperty("PW", "Player White")
  val HA = new SgfProperty("HA", "Handicap")
  val KM = new SgfProperty("KM", "Komi")
  val DT = new SgfProperty("DT", "Date")
  val TM = new SgfProperty("TM", "Time Limit")
  val RU = new SgfProperty("RU", "Rule Set")
  val B = new SgfProperty("B", "Black")
  val AB = new SgfProperty("AB", "Add Black")
  val W = new SgfProperty("W", "White")
  val AW = new SgfProperty("AW", "Add White")
  val C = new SgfProperty("C", "Comment")

  final case class SgfProperty(key: String, desc: String) extends Val
}
