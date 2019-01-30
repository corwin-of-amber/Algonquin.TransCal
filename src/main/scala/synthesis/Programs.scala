package synthesis

import com.typesafe.scalalogging.LazyLogging
import language.Language
import semantics.LambdaCalculus.isApp
import structures.immutable.HyperGraphManyWithOrderToOne
import structures._
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
  /** Builds trees from of programs where the hyper term is the base program and term conforms to pattern.
    *
    * @param hyperTermId The hyper term to build.
    * @param pattern graph pattern to limit output terms
    * @param root pattern root node
    * @return all conforming terms
    */
  def reconstructWithPattern(hyperTermId: HyperTermId, pattern: HyperPattern, root: TemplateTerm[HyperTermId]): Iterator[Term] = {
    val edgeTypes = pattern.edges.filter(_.target == root).map(_.edgeType.asInstanceOf[ExplicitTerm[HyperTermIdentifier]].value.identifier)
    reconstruct(hyperTermId).filter(t => edgeTypes.contains(t.root) && Programs.destruct(t).findSubgraph(pattern).nonEmpty)
  }

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
    Programs(hyperGraph ++ Programs.destruct(term, if (hyperGraph.nodes.isEmpty) HyperTermId(0) else hyperGraph.nodes.maxBy(_.id)))
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

  private def flattenApply(term: Term): (Identifier, List[Term]) = {
    if (term.root == Language.applyId && term.subtrees.head.root == Language.applyId) {
      val (fun, args) = flattenApply(term.subtrees.head)
      (fun, args ++ term.subtrees.tail)
    }
    else (term.root, term.subtrees)
  }

  private def innerDestruct[Node, EdgeType](tree: Term,
                                            nodeCreator: () => Node,
                                            identToEdge: Identifier => EdgeType,
                                            knownTerms: Term => Option[Node]): (Node, Set[HyperEdge[Node, EdgeType]]) = {
    if (knownTerms(tree).nonEmpty) return (knownTerms(tree).get, Set.empty)
    val (function, args) = flattenApply(tree)
    // Skipping annotations, shouldn't be part of the graph, at least for now
    if (function.literal == "Annotation") return innerDestruct(tree.subtrees(0), nodeCreator, identToEdge, knownTerms)
    val targetToSubedges = args.map(subtree => innerDestruct(subtree, nodeCreator, identToEdge, knownTerms))
    val subHyperEdges = targetToSubedges.flatMap(_._2).toSet
    val target = nodeCreator()
    val newHyperEdges =
      if (function == Language.splitId) targetToSubedges.map { t => HyperEdge(target, identToEdge(Language.idId), List(t._1), EmptyMetadata) }
      else Set(HyperEdge(target, identToEdge(function), targetToSubedges.map(_._1), EmptyMetadata))

    (target, subHyperEdges ++ newHyperEdges)
  }

  /** Create hyper graph from ast. Removes annotations. Root is always max HyperTermId.
    *
    * @param tree - program to be transformed into hypergraph
    * @param maxId - Max id so no ids will cross
    * @return
    */
  def destruct(tree: Term, maxId: HyperTermId=HyperTermId(0)): RewriteSearchState.HyperGraph = {
    destructWithRoot(tree, maxId)._1
  }

  def destructWithRoot(tree: Term, maxId: HyperTermId=HyperTermId(0)): (RewriteSearchState.HyperGraph, HyperTermId) = {
    logger.trace("Destruct a program")

    def knownTerms(t: Term): Option[HyperTermId] = None

    val hyperTermIdCreator = {
      val creator = Stream.from(maxId.id + 1).toIterator.map(HyperTermId)
      () => creator.next
    }

    val hyperEdges = innerDestruct(tree, hyperTermIdCreator, HyperTermIdentifier, knownTerms)._2
    (HyperGraphManyWithOrderToOne(hyperEdges.toSeq: _*), hyperEdges.last.target)
  }

  private val holeCreator = {
    // TODO: Hack to prevent intersection with known terms. Fix to something norml
    val creator = Stream.from(300).iterator
    () => ReferenceTerm[HyperTermId](creator.next())
  }

  private def innerDestructPattern(tree: Term, vars: Set[Set[Term]]):
  Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
    def edgeCreator(i: Identifier): TemplateTerm[HyperTermIdentifier] = ExplicitTerm(HyperTermIdentifier(i))

    val knownTerms: Term => Option[ReferenceTerm[HyperTermId]] = {
      val knownHoles: Map[Term, ReferenceTerm[HyperTermId]] = vars.zipWithIndex.flatMap(t => {
        val newHole = ReferenceTerm[HyperTermId](t._2)
        t._1.map((_, newHole))
      }).toMap
      t: Term => if (t.root.literal == "_") Some(holeCreator()) else knownHoles.get(t)
    }

    innerDestruct[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](tree, holeCreator, edgeCreator, knownTerms)._2
  }

  def destructPattern(tree: Term, vars: Set[Set[Term]]): HyperPattern = {
    val edges = innerDestructPattern(tree, vars)
    HyperGraphManyWithOrderToOne(edges.toSeq: _*)
  }

  def destructPatternWithRoot(tree: Term, vars: Set[Set[Term]]): (HyperPattern, ReferenceTerm[HyperTermId]) = {
    val edges = innerDestructPattern(tree, vars)
    (HyperGraphManyWithOrderToOne(edges.toSeq: _*),
      edges.last.target.asInstanceOf[ReferenceTerm[HyperTermId]])
  }

  private val arityEdges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val builtinToHyperTermId = language.Language.arity.keys.zip(Stream from 0 map HyperTermId).toMap
    val builtinEdges = builtinToHyperTermId.map(kv => HyperEdge(kv._2, HyperTermIdentifier(new Identifier(kv._1)), Seq.empty, EmptyMetadata))
    builtinEdges.toSet ++ language.Language.arity.map(kv => HyperEdge(builtinToHyperTermId("⊤"), HyperTermIdentifier(new Identifier(s"arity${kv._2}")), Seq(builtinToHyperTermId(kv._1)), EmptyMetadata))
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