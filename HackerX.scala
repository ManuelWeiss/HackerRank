object Solution {

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
    */

    val debugOn = true
    def debug(s: String) {
      if (debugOn)
        println(s)
    }

    case class Missile(t: Int, f: Int) {
      def fdist(that: Missile) = (this.f - that.f).abs
      def tdist(that: Missile) = (this.t - that.t).abs
      def withinRange(that: Missile) = fdist(that) <= tdist(that)
    }
    var ourMissiles = collection.mutable.HashSet[Missile]()

    def shootDown(x: Missile) = {
      debug(s"rocket $x coming in")
      if (directHit(x))
        debug(s"shot down $x with direct hit")
      else
        findClosest(x) match {
          case Some(m: Missile) => {
            ourMissiles -= m
            ourMissiles += x
            debug(s"found a close one: $m")
          }
          case None => {
            ourMissiles += x
            debug(s"none found, have to create one: $x")
          }
        }
      debug("current arsenal: " + ourMissiles.mkString(", "))
      debug("")
    }

    def directHit(x: Missile) = ourMissiles.exists(_.f == x.f)

    def findClosest(x: Missile): Option[Missile] = {
      val withinRange = ourMissiles.filter(_.withinRange(x))
      debug(s"within range: $withinRange")
      if (withinRange.isEmpty)
        None
      else
//        Some(withinRange.reduce((m, n) => if(m.fdist(x) <= n.fdist(x)) m else n)) // the one with the smallest freq distance
//        Some(withinRange.reduce((m, n) => if(m.t >= n.t) m else n)) // the youngest
      Some(withinRange.reduce((m, n) => if(m.t < n.t) m else n)) // the oldest
    }

    val NoOfMissiles = Console.readLine.toInt
    val incomingMissiles = for (i <- 1 to NoOfMissiles)
      yield {
        val pair = Console.readLine.split(" ").map(_.toInt)
        Missile(pair(0), pair(1))
      }

    incomingMissiles foreach {x => shootDown(x)}

    println(ourMissiles.size)
  }
}