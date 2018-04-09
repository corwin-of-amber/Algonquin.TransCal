package relentless.matching

import com.typesafe.scalalogging.LazyLogging
import relentless.rewriting.HyperEdge
import report.data.NumeratorWithMap
import syntax.AstSugar.Term
import syntax.Tree


class Encoding extends LazyLogging {

  import semantics.LambdaCalculus.isApp

  val ntor = new NumeratorWithMap { }

  def toTuples(term: Term) : List[HyperEdge[Int]] = toTuples(term, term)

  /**
    * Like toTuples(term), but encodes the top-level node according to equateWith, rather than term.
    */
  def toTuples(term: Term, equateWith: Term) : List[HyperEdge[Int]] = {
    val top = equateWith
    val (head, rest) = headRest(term)
    toTuple(Seq(head, top) ++ rest.toSeq) :: (rest flatMap toTuples)
  }

  /**
    * Like toTuples(term), but encodes the top-level node according to equateWith, rather than term (int version).
    */
  def toTuples(term: Term, equateWith: Int) : List[HyperEdge[Int]] = {
    val top = equateWith
    val (head, rest) = headRest(term)
    val tail = rest flatMap toTuples
    val firstHalf = HyperEdge((ntor --> head) +: top +: toTuple(rest.toSeq))
    firstHalf :: tail
  }

  def toTuples(term: Term, alt: Map[Term, Int], nholes: Int, atRoot: Boolean = true) : List[Array[Int]] = {
    if (!atRoot && alt.contains(term) && alt(term) >= ~nholes && term.isLeaf) List.empty
    else {
      val (head, rest) = headRest(term)
      ((ntor --> head) +: toTuple(Seq(term) ++ rest.toSeq, alt)) :: (rest flatMap (toTuples(_, alt, nholes, false)))
    }
  }

  /**
    * Encodes a term with "holes": these are assigned negative integers ~1 through ~holes.length.
    * Subterms are considered implicit holes, so they are assigned distinct negative values.
    * The roots of the terms in the bundle are special: they are all encoded as ~0.
    */
  def toBundle(holes: Term*)(terms: Term*) = {
    def dbg(x: List[Array[Int]]) { logger.info(s"${x map (_ map (x => if (x < 0) x else (ntor <-- x)) mkString " ")}") }

    val altsq = terms.head :: holes.toList ++ (terms flatMap (term => term.nodes filterNot (n => (n eq term) || n.isLeaf)))
    val alt = altsq.zipWithIndex.toMap.mapValues(~_) ++ (terms.tail map ((_, ~0)))
    new Bundle((terms flatMap (toTuples(_, alt, holes.length)) toList))// |-- dbg)
  }

  /**
    * Encodes several bundles together in a way that does not induce encoding collisions
    * for the placeholder elements.
    *
    * The first bundle is translated normally with its root encoded as ~0.
    * The holes are encoded ~1 through ~holes.length.
    * Remaining inner terms and bundle roots are encoded as ~(holes.length+1) onward.
    *
    * NOTICE BIG This function is currently used for rule patterns, whereas toBundle is used
    *   for regular terms and conclusion schemes. As a result, the `alt` set of terms mapped
    *   to negative integers include (non-hole) leaves such as "tt" and "nil"; see (*) below.
    *   Be aware of this small difference when calling them.
    */
  def toBundles(holes: Term*)(terms: List[List[Term]]) = {
    val altsq = terms.head.head :: holes.toList ++
      (terms.flatten flatMap (term => term.nodes filterNot (n => (n eq term) || (holes contains n) /* (*) */ /*n.isLeaf*/)))
    val alt = altsq.zipWithIndex.toMap.mapValues(~_) ++ (terms.head.tail map ((_, ~0))) ++
      (terms.tail.zipWithIndex flatMap { case (terms, i) => terms map ((_, ~(altsq.length + i))) })
    new Bundle((terms.flatten flatMap (toTuples(_, alt, holes.length)) toList))// |-- dbg)
  }

  def headRest(term: Term) = {
    isApp(term) match {
      case Some((f, args)) if f.isLeaf => (f.leaf, args)
      case _ => (term.root, term.subtrees)
    }
  }

  def toTuple(sq: Seq[AnyRef]) = HyperEdge(sq map (ntor -->))
  def toTuple(sq: Seq[Term], alt: Map[Term, Int]) = sq map (k => alt.getOrElse(k, ntor --> k)) toArray

  def asTerm(n: Int) = (ntor <-- n) match {
    case t: Tree[_] => t.asInstanceOf[Term] // hopefully there are no other trees
    //case _ => T(I("some error", "marker"))
  }
}
