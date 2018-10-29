package relentless.matching

import relentless.matching.structures.filling.{HyperTerm, ImplPattern, Pattern, Placeholder}
import relentless.rewriting.HyperEdge

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A sequence of tuples, possibly with holes denoted by negative integers.
  */
class Bundle(val patterns: List[Pattern]) {

  def this(patterns: List[Array[Int]], holes: Int*) = //this(tuples)
    this(Bundle.puncture(patterns, holes.toList) map (Pattern.toHyperTermBase(_)) map (new ImplPattern(_)))

  private def fillIn(pattern: Pattern, args: Int*): IndexedSeq[Int] =
    pattern map {
      case Placeholder(v) => args(v)
      case HyperTerm(v) => v
    }

  private def fillIn(args: Int*): List[IndexedSeq[Int]] =
    patterns map (pattern => fillIn(pattern, args: _*))

  lazy val minValuationSize: Int = {
    val places = patterns.flatMap(a => a.placeholders)
    if (places.nonEmpty) places.map(_.value).max + 1 else 0
  }

  // If this became slow manually do exists
  def bare: Bundle = new Bundle(patterns filter (_.placeholders.nonEmpty))

  /**
    *
    * @return
    */
  def shuffles: List[Bundle] = {
    patterns.indices map { i =>
      val inputBuffer = new ListBuffer ++ patterns
      val outputBuffer = new ListBuffer[Pattern]

      // set of placeholders already encountered
      val covered = mutable.Set.empty[Placeholder]
      var j = i
      while (inputBuffer.nonEmpty) {
        val next = inputBuffer(j)
        inputBuffer.remove(j)
        covered ++= next.placeholders
        outputBuffer += next
        // - find the next spot that shares at least one value with covered
        if (inputBuffer.nonEmpty) {
          j = inputBuffer indexWhere (_.placeholders exists covered.contains)
          if (j < 0) j = 0
          //throw new RuntimeException("bundle is not connected: " + (tuples map (_ mkString ",") mkString " "))
        }
      }
      new Bundle(outputBuffer.toList)
    } toList
  }

  def shuffles2: List[Bundle] = {
    patterns.indices map { i =>
      val inputBuffer = new ListBuffer ++ patterns
      val outputBuffer = new ListBuffer[Pattern]
      val covered = mutable.Set.empty[Placeholder] // set of placeholders already encountered
    var j = i
      while (inputBuffer.nonEmpty) {
        val next = inputBuffer(j)
        inputBuffer.remove(j)
        covered ++= next.placeholders
        outputBuffer += next
        // - find the next spot that shares at least one value with covered
        if (inputBuffer.nonEmpty) {
          j = inputBuffer indexWhere (_.placeholders exists covered.contains)
          if (j < 0) j = 0
          //throw new RuntimeException("bundle is not connected: " + (tuples map (_ mkString ",") mkString " "))
        }
      }
      new Bundle(outputBuffer.toList)
    } toList
  }
}

object Bundle {

  def puncture(patterns: List[Array[Int]], holes: List[Int]): List[HyperEdge[Int]] =
    patterns map (tp => puncture(tp, holes)) filterNot (_ (1) <= -2)

  // drop hole leaves (_(1) <= -2)

  private def puncture(pattern: Array[Int], holes: List[Int]): HyperEdge[Int] =
    HyperEdge(pattern map (x => holes.indexOf(x) match {
      case i if i >= 0 => -i - 2
      case _ => x
    }))

  // note: -1 is reserved, holes are -2 and below
}
