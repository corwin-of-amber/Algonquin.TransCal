package synthesis

import com.typesafe.scalalogging.LazyLogging
import semantics.LambdaCalculus.isApp
import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge}
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import synthesis.rewrites.RewriteRule.{HyperPattern, HyperPatternEdge}
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}

import scala.collection.mutable

/** Programs contains all the available programs holding them for future optimized rewrites and reconstruction of them.
  *
  * @author tomer
  * @since 11/19/18
  */
class Programs private(val hyperGraph: HyperGraph) extends LazyLogging {

  /* --- Public --- */

  /** Builds trees from of programs where the hyper term is the base program.
    *
    * @param hyperTermId The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTermId: HyperTermId): Iterator[Term] = {
    logger.trace("Reconstruct programs")

    if (!hyperGraph.nodes.contains(hyperTermId)) {
      logger.debug(f"Unknown HyperTerm - $hyperTermId")
      Iterator.empty
    } else {
      val hyperTermToEdge = mutable.HashMultiMap(hyperGraph.edges.groupBy(edge => edge.target))

      /** Build iterator of program's trees where their root is the current target.
        *
        * @param root The root of the programs we find
        * @return Iterator with all the programs of root.
        */
      def recursive(root: HyperTermId): Iterator[Term] = {
        val edges = hyperTermToEdge.get(root)
        edges.map(edges => edges.filter(_.edgeType.identifier.kind != Programs.Kinds.NonConstructable.toString).toIterator.flatMap(edge => {
          if (edge.sources.isEmpty) Iterator(new Tree[Identifier](edge.edgeType.identifier))
          else Programs.combineSeq(edge.sources.map(recursive)).map(subtrees => new Tree[Identifier](edge.edgeType.identifier, subtrees.toList))
        })).get
      }

      recursive(hyperTermId)
    }
  }

  /** Adds a new term to the programs.
    *
    * @param term The new term to add.
    * @return New programs with the term in it.
    */
  def addTerm(term: Term): Programs = {
    Programs(hyperGraph ++ Programs.destruct(term))
  }

  def +(term: Term): Programs = addTerm(term)


  /* --- Object Impl. --- */

  override def toString: String = f"Programs($hyperGraph)"
}

object Programs extends LazyLogging {

  /* --- Public --- */
  object Kinds extends Enumeration {
    val Constructable, NonConstructable = Value
  }

  def empty: Programs = Programs(HyperGraphManyWithOrderToOne.empty[HyperTermId, HyperTermIdentifier])

  def apply(hyperGraph: RewriteSearchState.HyperGraph): Programs = new Programs(hyperGraph)

  def apply(tree: Term): Programs = Programs(Programs.destruct(tree))

  private def flattenApply(term: Term) = {
    if (term.root.literal == "@") (term.subtrees.head.root, term.subtrees.tail)
    else (term.root, term.subtrees)
  }

  protected def destructPatternVars(hyperGraph: HyperGraph) = {
    val holeEdges = hyperGraph.filter(e => e.edgeType.identifier.literal.toString.startsWith("?"))
    val holePartners = {
      val edgeTypes = holeEdges.edgeTypes.map(i => HyperTermIdentifier(new Identifier(i.identifier.literal.toString.drop(1), i.identifier.kind, i.identifier.ns)))
      hyperGraph.filter(e => edgeTypes.contains(e.edgeType)).nodes
    }
    val holes = holeEdges.nodes
    val references = holes.zip(holePartners).zipWithIndex.flatMap { tup => {
      val h = tup._1._1
      val hp = tup._1._2
      val ref = ReferenceTerm[HyperTermId](tup._2)
      Map(h -> ref, hp -> ref)
    }}.toMap
    HyperGraphManyWithOrderToOne(hyperGraph.filter(e => !references.contains(e.target)).map(e =>
      new HyperPatternEdge(ExplicitTerm(e.target), ExplicitTerm(e.edgeType), e.sources.map( i => references.getOrElse(i, ExplicitTerm(i))), e.metadata)
    ).toSeq:_*)
  }

  protected def destructMissingVars(hyperGraph: HyperPattern): HyperPattern = {
    var maxRef = hyperGraph.nodes.map { case ReferenceTerm(i) => i; case _ => 0 }.max

    val sourcesToHoles: Map[TemplateTerm[HyperTermId], ReferenceTerm[HyperTermId]] = {
      val edges = hyperGraph.findEdges(ExplicitTerm(HyperTermIdentifier(new Identifier("_"))))
      edges.zip(Stream from maxRef+1).map( t => (t._1.target, ReferenceTerm[HyperTermId](t._2))).toMap
    }

    HyperGraphManyWithOrderToOne(hyperGraph.filter(e => !sourcesToHoles.contains(e.target)).map(e =>
      new HyperPatternEdge(e.target, e.edgeType, e.sources.map(i => sourcesToHoles.getOrElse(i, i)), e.metadata)
    ).toSeq:_*)
  }

  def destructPattern(tree: Term): HyperPattern = {
    val graph = destruct(tree)
    val removedVars = destructPatternVars(graph)
    destructMissingVars(removedVars)
  }

  private val hyperTermIdCreator = {
    val creator = Stream.from(language.arity.size).iterator
    () => creator.next()
  }

  private val arityEdges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val builtinToHyperTermId = language.arity.keys.zip(Stream from 0 map HyperTermId).toMap
    val builtinEdges = builtinToHyperTermId.map(kv => HyperEdge(kv._2, HyperTermIdentifier(new Identifier(kv._1)), Seq.empty, EmptyMetadata))
    builtinEdges.toSet ++ language.arity.map(kv => HyperEdge(builtinToHyperTermId("⊤"), HyperTermIdentifier(new Identifier(s"arity${kv._2}")), Seq(builtinToHyperTermId(kv._1)), EmptyMetadata))
  }

  def destruct(tree: Term): RewriteSearchState.HyperGraph = {
    logger.trace("Destruct a program")

    def innerDestruct(tree: Term): (HyperTermId, Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) = {
      val (function, args) = flattenApply(tree)
      val targetToSubedges = args.map(subtree => innerDestruct(subtree))
      val subHyperEdges = targetToSubedges.flatMap(_._2).toSet
      val target = HyperTermId(hyperTermIdCreator())
      val newHyperEdges =
        if (function.literal == "/") targetToSubedges.map { t => HyperEdge(target, HyperTermIdentifier(new Identifier("id")), List(t._1), EmptyMetadata) }
        else Set(HyperEdge(target, HyperTermIdentifier(function), targetToSubedges.map(_._1), EmptyMetadata))

      (target, subHyperEdges ++ newHyperEdges)
    }

    val hyperEdges = innerDestruct(tree)._2

    HyperGraphManyWithOrderToOne(hyperEdges.toSeq:_*)
  }

  /** Iterator which combines sequence of iterators (return all combinations of their results).
    *
    * @param iterators All the iterators to combine.
    * @tparam T The return type.
    */
  def combineSeq[T](iterators: Seq[Iterator[T]]): Iterator[Seq[T]] = {
    iterators match {
      case Nil => Iterator.empty
      case head +: Nil => head.map(Seq(_))
      case head +: tail => combineTwo(head, combineSeq(tail)).map(t => t._1 +: t._2)
    }
  }

  /** Iterator which combines two iterators (return all combinations of their results).
    *
    * @param iter1 First iterator.
    * @param iter2 Second iterator.
    * @tparam A The first type
    * @tparam B The second type
    */
  private def combineTwo[A, B](iter1: Iterator[A], iter2: Iterator[B]): Iterator[(A, B)] = {
    iter1.flatMap(elem1 => iter2.duplicate._2.map(elem2 => (elem1, elem2)))
  }
}