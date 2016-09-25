package relentless.matching

import syntax.AstSugar._
import report.data.NumeratorWithMap
import scala.collection.mutable.ListBuffer



class Encoding {
  
  import semantics.LambdaCalculus.isApp
  
  val ntor = new NumeratorWithMap { }
  
  def toTuples(term: Term) : List[Array[Int]] = toTuples(term, term)
  
  /**
   * Like toTuples(term), but encodes the top-level node according to equateWith, rather than term.
   */
  def toTuples(term: Term, equateWith: Term) : List[Array[Int]] = {
    val top = equateWith
    val (head, rest) = headRest(term)
    toTuple(Seq(head, top) ++ rest.toSeq) :: (rest flatMap toTuples)
  }

  /**
   * Like toTuples(term), but encodes the top-level node according to equateWith, rather than term (int version).
   */
  def toTuples(term: Term, equateWith: Int) : List[Array[Int]] = {
    val top = equateWith
    val (head, rest) = headRest(term)
    ((ntor --> head) +: top +: toTuple(rest.toSeq)) :: (rest flatMap toTuples)
  }

  def toTuples(term: Term, alt: Map[Term, Int]) : List[Array[Int]] = {
    if (alt.contains(term) && alt(term) != -1 && term.isLeaf) List.empty
    else {
      val (head, rest) = headRest(term)
      ((ntor --> head) +: toTuple(Seq(term) ++ rest.toSeq, alt)) :: (rest flatMap (toTuples(_, alt)))
    }
  }

  def toBundle(holes: Term*)(terms: Term*) = {
    import syntax.Piping._
    def dbg(x: List[Array[Int]]) { println(x map (_ map (x => if (x < 0) x else (ntor <-- x)) mkString " ")) }
    
    val altsq = terms.head :: holes.toList ++ (terms flatMap (term => term.nodes filterNot (n => (n eq term) || n.isLeaf)))
    val alt = altsq.zipWithIndex.toMap.mapValues(~_) ++ (terms.tail map ((_, ~0)))
    new Bundle((terms flatMap (toTuples(_, alt)) toList))// |-- dbg)
  }
  
  def headRest(term: Term) = {
    isApp(term) match {
      case Some((f, args)) if f.isLeaf => (f.leaf, args)
      case _ => (term.root, term.subtrees)
    }
  }
  
  def toTuple(sq: Seq[AnyRef]) = sq map (ntor -->) toArray
  def toTuple(sq: Seq[Term], alt: Map[Term, Int]) = sq map (k => alt.getOrElse(k, ntor --> k)) toArray

  def asTerm(n: Int) = (ntor <-- n).asInstanceOf[Term]

}


/**
 * A sequence of tuples, possibly with holes denoted by negative integers.
 */
class Bundle(val tuples: List[Array[Int]]) {
  
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
          if (j < 0) throw new RuntimeException("bundle is not connected: " + (tuples map (_ mkString ",") mkString " "))
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





