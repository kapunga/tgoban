package org.kapunga.tgoban


/**
 * Created by kapunga on 6/7/14.
 */
class Game(size: Int, handicap: Int, komi: Float) {

}

abstract class Move
case class Handicap(num: Int) extends Move
case class White()


