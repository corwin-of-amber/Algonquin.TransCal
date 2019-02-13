package synthesis

import com.typesafe.scalalogging.LazyLogging
import language.Language
import structures.immutable.{CompactHyperGraph, HyperGraphManyWithOrderToOne}
import structures._
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import synthesis.Programs.NonConstructableMetadata
import synthesis.rewrites.RewriteRule.HyperPattern
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
    * @param pattern     graph pattern to limit output terms
    * @param root        pattern root node
    * @return all conforming terms
    */
  def reconstructWithPattern(hyperTermId: HyperTermId, pattern: HyperPattern): Iterator[Term] = {
    reconstruct(hyperTermId).filter(t => Programs.destruct(t).findSubgraph(pattern).nonEmpty)
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
        edges.map(edges => edges.filter(_.metadata.forall(_ != NonConstructableMetadata)).toIterator.flatMap(edge => {
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
  object NonConstructableMetadata extends Metadata {
    override protected def toStr: String = "Non Constructable"
  }

  def empty: Programs = Programs(CompactHyperGraph.empty[HyperTermId, HyperTermIdentifier])

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
    var target = nodeCreator()

    val newHyperEdges = function match {
      case Language.splitId | Language.andCondBuilderId =>
        val createdTarget = target
        target = targetToSubedges.head._1
        targetToSubedges.map {t => HyperEdge(t._1, identToEdge(Language.idId), List(createdTarget), NonConstructableMetadata)}
      case Language.trueCondBuilderId =>
        val precondRoot = targetToSubedges.head._1
//        val precondEdge = targetToSubedges.head._2.find(_.target == precondRoot).get
        target = targetToSubedges.last._1
        Set(
          HyperEdge(precondRoot, identToEdge(Language.trueId), List.empty, EmptyMetadata)
        )
      case _ => Set(HyperEdge(target, identToEdge(function), targetToSubedges.map(_._1), EmptyMetadata))
    }

    (target, subHyperEdges ++ newHyperEdges)
  }

  /** Create hyper graph from ast. Removes annotations. Root is always max HyperTermId.
    *
    * @param tree  - program to be transformed into hypergraph
    * @param maxId - Max id so no ids will cross
    * @return
    */
  def destruct(tree: Term, maxId: HyperTermId = HyperTermId(0)): RewriteSearchState.HyperGraph = {
    destructWithRoot(tree, maxId)._1
  }

  private def destructWithRoot(tree: Term, maxId: HyperTermId = HyperTermId(0)): (RewriteSearchState.HyperGraph, HyperTermId) = {
    logger.trace("Destruct a program")

    def knownTerms(t: Term): Option[HyperTermId] = None

    val hyperTermIdCreator = {
      val creator = Stream.from(maxId.id + 1).toIterator.map(HyperTermId)
      () => creator.next
    }

    val hyperEdges = innerDestruct(tree, hyperTermIdCreator, HyperTermIdentifier, knownTerms)._2
    (CompactHyperGraph(hyperEdges.toSeq: _*), hyperEdges.last.target)
  }

  private def innerDestructPattern(trees: Seq[Term]):
  Seq[(TemplateTerm[HyperTermId], Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]])] = {
    def edgeCreator(i: Identifier): TemplateTerm[HyperTermIdentifier] = ExplicitTerm(HyperTermIdentifier(i))

    val holeCreator: () => ReferenceTerm[HyperTermId] = {
      val creator = Stream.from(0).iterator
      () => ReferenceTerm[HyperTermId](creator.next())
    }

    val knownTerms: Term => Option[ReferenceTerm[HyperTermId]] = {
      val knownHoles: Map[Term, ReferenceTerm[HyperTermId]] = {
        val vars = trees.flatMap(t => t.leaves.filter(_.root.literal.toString.startsWith("?"))).map(t =>
          Set(t, new Tree(new Identifier(t.root.literal.toString.drop(1), t.root.kind, t.root.ns)))
        )
        vars.flatMap(s => {
          val newHole: ReferenceTerm[HyperTermId] = holeCreator()
          Set((s.head, newHole), (s.last, newHole))
        })
      }.toMap
      t: Term => if (t.root.literal == "_") Some(holeCreator()) else knownHoles.get(t)
    }

    trees.map(tree => innerDestruct[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](tree, holeCreator, edgeCreator, knownTerms))
  }

  def destructPattern(tree: Term): HyperPattern = {
    destructPatternsWithRoots(Seq(tree)).head._1
  }

  def destructPatterns(trees: Seq[Term], mergeRoots: Boolean = true): Seq[HyperPattern] = {
    destructPatternsWithRoots(trees, mergeRoots).map(_._1)
  }

  private def destructPatternsWithRoots(trees: Seq[Term], mergeRoots: Boolean = true): Seq[(HyperPattern, TemplateTerm[HyperTermId])] = {
    val edges = innerDestructPattern(trees)
    val modifiedEdges: Seq[(TemplateTerm[HyperTermId], Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]])] = {
      val mainRoot = edges.head._1
      val roots = edges.map(_._1).toSet
      def switcher(templateTerm: TemplateTerm[HyperTermId]) = if (roots.contains(templateTerm)) mainRoot else templateTerm
      if (mergeRoots) edges.map(es => (mainRoot, es._2.map(e => e.copy(target = switcher(e.target), sources = e.sources.map(switcher)))))
      else edges
    }
    modifiedEdges.map(es => (HyperGraphManyWithOrderToOne(es._2.toSeq: _*), es._1))
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