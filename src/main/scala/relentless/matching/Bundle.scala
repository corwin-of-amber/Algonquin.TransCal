package relentless.matching

import scala.collection.mutable.ListBuffer


/**
 * A sequence of tuples, possibly with holes denoted by negative integers.
 */
class Bundle(val tuples: List[Array[Int]]) {
  val patterns: List[Pattern] = tuples map (Pattern.toHyperTermBase(_)) map (new Pattern(_))

  def this(tuples: List[Array[Int]], holes: Int*) = //this(tuples)
    this(Bundle.puncture(tuples, holes.toList))
  
  def fillIn(tuple: Array[Int], args: Int*) =
    tuple map (x => if (x < 0) args(~x) else x)
  
  def fillIn(args: Int*): List[Array[Int]] =
    tuples map (tp => fillIn(tp, args:_*))

  lazy val minValuationSize = tuples.flatten.toSeq.min match {
    case x if x < 0 => ~x + 1
    case _ => 0
  }
  
  def bare = new Bundle(tuples filter (_ exists (_ < 0)))
  
  import collection.mutable
  
  def shuffles = {
    0 until tuples.length map { i =>
      val inb = new ListBuffer ++ tuples
      val outb = new ListBuffer[Array[Int]]
      val covered = mutable.Set.empty[Int]  // set of placeholders already encountered
      var j = i
      while (inb.nonEmpty) {
        val next = inb(j)
        inb.remove(j)
        covered ++= next filter (_ < 0)
        outb += next
        // - find the next spot that shares at least one value with covered
        if (inb.nonEmpty) {
          j = inb indexWhere (_ exists covered.contains)
          if (j < 0) j = 0
            //throw new RuntimeException("bundle is not connected: " + (tuples map (_ mkString ",") mkString " "))
        }
      }
      new Bundle(outb.toList)
    } toList
  }
}

object Bundle {
  
  def puncture(tuples: List[Array[Int]], holes: List[Int]): List[Array[Int]] =
    tuples map (tp => puncture(tp, holes)) filterNot (_(1) <= -2)
    // drop hole leaves (_(1) <= -2)
  
  def puncture(tuple: Array[Int], holes: List[Int]): Array[Int] =
    tuple map (x => holes.indexOf(x) match { case i if i >= 0 => -i - 2 case _ => x })
    // note: -1 is reserved, holes are -2 and below
}
