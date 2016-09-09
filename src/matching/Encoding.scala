package matching

import syntax.AstSugar._
import report.data.NumeratorWithMap



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

  def toBundle(term: Term, holes: Term*) = new Bundle(toTuples(term, -1), (holes map (ntor -->)):_*)
  
  def headRest(term: Term) = {
    isApp(term) match {
      case Some((f, args)) if f.isLeaf => (f.leaf, args)
      case _ => (term.root, term.subtrees)
    }
  }
  
  def toTuple(sq: Seq[AnyRef]) = sq map (ntor -->) toArray

  def asTerm(n: Int) = (ntor <-- n).asInstanceOf[Term]

}


/**
 * A sequence of tuples, possibly with holes denoted by negative integers.
 */
class Bundle(val tuples: List[Array[Int]]) {
  
  def this(tuples: List[Array[Int]], holes: Int*) = //this(tuples)
    this(Bundle.puncture(tuples, holes.toList))
  
  def fillIn(tuple: Array[Int], args: Int*) =
    tuple map (x => if (x < 0) args(-x - 1) else x)
  
  def fillIn(args: Int*): List[Array[Int]] =
    tuples map (tp => fillIn(tp, args:_*))

}

object Bundle {
  
  def puncture(tuples: List[Array[Int]], holes: List[Int]): List[Array[Int]] =
    tuples map (tp => puncture(tp, holes)) filterNot (_(1) <= -2)
    // drop hole leaves (_(1) <= -2)
  
  def puncture(tuple: Array[Int], holes: List[Int]): Array[Int] =
    tuple map (x => holes.indexOf(x) match { case i if i >= 0 => -i - 2 case _ => x })
    // note: -1 is reserved, holes are -2 and below
}





