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

  /** Convert a term to a bunch of Patterns. Each pattern represents a connection between a parent and its children in
    * the @term. So head is the edge type, rest is params and term is the target.
    *
    * @param term the term to translate
    * @param termToPlaceholder convert known terms (actually hyper terms with holes) to their corresponding placeholders
    * @param nholes number of holes expected
    * @param atRoot for recursive purposes
    * @return
    */
  def toPatterns(term: Term, termToPlaceholder: Map[Term, Placeholder], nholes: Int, atRoot: Boolean = true): List[Pattern] = {
    if (!atRoot && termToPlaceholder.contains(term) && termToPlaceholder(term).value <= nholes && term.isLeaf) List.empty
    else {
      val (head, rest) = headRest(term)
      new Pattern(Pattern.toHyperTermBase(this --> head) +: toTuple(Seq(term) ++ rest.toSeq, termToPlaceholder) toIndexedSeq) ::
        (rest flatMap (toPatterns(_, termToPlaceholder, nholes, false)))
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

    assert(holes.forall(_.isLeaf))
    val allHoles = {
      // All terms other then the holes and the root need to become holes (they need a new hyper term)
      def internalSubterms(root: Term): Stream[Term] = root.nodes filter (subterm => !((subterm eq root) || subterm.isLeaf))
      terms.head :: holes.toList ++ (terms flatMap internalSubterms)
    }

    val termToPlaceholder = allHoles.zipWithIndex.toMap.mapValues(Placeholder) ++ (terms.tail map ((_, Placeholder(0))))
    new Bundle(terms flatMap (toPatterns(_, termToPlaceholder, holes.length)) toList) // |-- dbg)
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
    // All holes including the addition of hyperterms for the subterms in the bundle
    val allHoles = terms.head.head :: holes.toList ++
      (terms.flatten flatMap (term => term.nodes filterNot (n => (n eq term) || (holes contains n)/* (*) */
        /*n.isLeaf*/))) distinct
    val termToPlaceholder: Map[Term, Placeholder] =
      allHoles.zipWithIndex.toMap.mapValues(Placeholder) ++ (terms.head.tail map ((_, Placeholder(0)))) ++
      (terms.tail.zipWithIndex flatMap { case (terms, i) => terms map ((_, Placeholder(allHoles.length + i))) })
    new Bundle(terms.flatten flatMap (toPatterns(_, termToPlaceholder, holes.length)) toList) // |-- dbg)
  }

  private def headRest(term: Term) = {
    isApp(term) match {
      case Some((f, args)) if f.isLeaf => (f.leaf, args)
      case _ => (term.root, term.subtrees)
    }
  }

  private def toTuple(sq: Seq[Term]) = sq map (this --> _)

  private def toTuple(sq: Seq[Term], termToPlaceholder: Map[Term, Placeholder]): Seq[BaseHyperTerm] =
    sq map (k => termToPlaceholder.getOrElse(k, HyperTerm(this --> k)))
}
