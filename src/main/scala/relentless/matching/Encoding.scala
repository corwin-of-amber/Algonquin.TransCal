package relentless.matching

import com.typesafe.scalalogging.LazyLogging
import relentless.rewriting.{BaseHyperEdge, HyperEdge, OriginalEdge}
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}


class Encoding extends LazyLogging {

  import semantics.LambdaCalculus.isApp

  private val identifierMap: collection.mutable.Map[Identifier, Int] = collection.mutable.Map.empty
  private var max = 0

  private val termMap: collection.mutable.Map[Term, Int] = collection.mutable.Map.empty

  def -->(value: Identifier): Int = {
    if (value == null) throw new Exception("Received null identifier")
    identifierMap get value match {
      case Some(idx) => idx
      case _ => max = max + 1; identifierMap += (value -> max); max
    }
  }

  def <--(index: Int): Option[Identifier] = identifierMap find (_._2 == index) match {
    case Some((ns, _)) => Some(ns)
    case _ => None
  }

  def -->(value: Term): Int = {
    if (value == null) throw new Exception("Received null term")
    termMap get value match {
      case Some(idx) => idx
      case _ => max = max + 1; termMap += (value -> max); max
    }
  }

  def reserveIndex(): Int = { max = max + 1;  max }

  def toOriginalEdges(term: Term): List[OriginalEdge[Int]] = toOriginalEdges(term, term)

  /**
    * Like toTuples(term), but encodes the top-level node according to equateWith, rather than term.
    */
  def toOriginalEdges(term: Term, equateWith: Term): List[OriginalEdge[Int]] = {
    val top = this --> equateWith
    val (head, rest): (Identifier, List[Term]) = headRest(term)
    new OriginalEdge[Int](this --> head, top, toTuple(rest)) :: (rest flatMap toOriginalEdges)
  }

  /**
    * Like toTuples(term), but encodes the top-level node according to equateWith, rather than term (int version).
    */
  def toOriginalEdges(term: Term, equateWith: Int): List[OriginalEdge[Int]] = {
    val top = equateWith
    val (head, rest) = headRest(term)
    val tail = rest flatMap toOriginalEdges
    val firstHalf = OriginalEdge(this --> head, top, toTuple(rest.toSeq))
    firstHalf :: tail
  }

  def toHyperEdges(term: Term, alt: Map[Term, Int], nholes: Int, atRoot: Boolean = true): List[HyperEdge[Int]] = {
    if (!atRoot && alt.contains(term) && alt(term) >= ~nholes && term.isLeaf) List.empty
    else {
      val (head, rest) = headRest(term)
      HyperEdge((this --> head) +: toTuple(Seq(term) ++ rest.toSeq, alt)) :: (rest flatMap (toHyperEdges(_, alt, nholes, false)))
    }
  }

  /**
    * Encodes a term with "holes": these are assigned negative integers ~1 through ~holes.length.
    * Subterms are considered implicit holes, so they are assigned distinct negative values.
    * The roots of the terms in the bundle are special: they are all encoded as ~0.
    */
  def toBundle(holes: Term*)(terms: Term*) = {
    def dbg(x: List[Array[Int]]) {
      logger.info(s"${x map (_ map (x => if (x < 0) x else this <-- x) mkString " ")}")
    }

    val altsq = terms.head :: holes.toList ++ (terms flatMap (term => term.nodes filterNot (n => (n eq term) || n.isLeaf)))
    val alt = altsq.zipWithIndex.toMap.mapValues(~_) ++ (terms.tail map ((_, ~0)))
    new Bundle(terms flatMap (toHyperEdges(_, alt, holes.length)) toList) // |-- dbg)
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
    * for regular terms and conclusion schemes. As a result, the `alt` set of terms mapped
    * to negative integers include (non-hole) leaves such as "tt" and "nil"; see (*) below.
    * Be aware of this small difference when calling them.
    */
  def toBundles(holes: Term*)(terms: List[List[Term]]) = {
    val altsq = terms.head.head :: holes.toList ++
      (terms.flatten flatMap (term => term.nodes filterNot (n => (n eq term) || (holes contains n) /* (*) */
        /*n.isLeaf*/)))
    val alt = altsq.zipWithIndex.toMap.mapValues(~_) ++ (terms.head.tail map ((_, ~0))) ++
      (terms.tail.zipWithIndex flatMap { case (terms, i) => terms map ((_, ~(altsq.length + i))) })
    new Bundle(terms.flatten flatMap (toHyperEdges(_, alt, holes.length)) toList) // |-- dbg)
  }

  private def headRest(term: Term) = {
    isApp(term) match {
      case Some((f, args)) if f.isLeaf => (f.leaf, args)
      case _ => (term.root, term.subtrees)
    }
  }

  private def toTuple(sq: Seq[Term]) = sq map (this --> _)

  private def toTuple(sq: Seq[Term], alt: Map[Term, Int]) = sq map (k => alt.getOrElse(k, this --> k))
}
