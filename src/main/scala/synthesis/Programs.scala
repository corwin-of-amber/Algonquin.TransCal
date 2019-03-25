package synthesis

import com.typesafe.scalalogging.LazyLogging
import transcallang.Language
import structures.immutable.{CompactHyperGraph, HyperGraphManyWithOrderToOne, VersionedHyperGraph, VocabularyHyperGraph}
import structures._
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import synthesis.Programs.NonConstructableMetadata
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.{RewriteSearchState, Template}
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}

import scala.collection.mutable

/** Programs contains all the available programs holding them for future optimized rewrites and reconstruction of them.
  *
  * @author tomer
  * @since 11/19/18
  */
class Programs(val hyperGraph: HyperGraph) extends LazyLogging {

  /* --- Public --- */
  /** Builds trees from of programs where the hyper term is the base program and term conforms to pattern.
    *
    * @param hyperTermId The hyper term to build.
    * @param pattern     graph pattern to limit output terms
    * @param root        pattern root node
    * @return all conforming terms
    */
  def reconstructWithPattern(hyperTermId: HyperTermId, pattern: HyperPattern, patternRoot: Option[TemplateTerm[HyperTermId]] = None): Iterator[Term] = {
//    reconstruct(hyperTermId).filter(t => {
//      val (destroyedGraph, destroyedRoot) = Programs.destructWithRoot(t)
//      val updatedPattern = patternRoot.map(pattern.mergeNodes(ExplicitTerm(destroyedRoot), _)).getOrElse(pattern)
//      destroyedGraph.findSubgraph[Int](updatedPattern).nonEmpty
//    })

    val newPattern = patternRoot.map(pattern.mergeNodes(ExplicitTerm(hyperTermId), _)).getOrElse(pattern)
    val maps = hyperGraph.findSubgraph[Int](newPattern)
    maps.toIterator.flatMap(m => {
      val fullPattern = HyperGraphManyWithOrderToOneLike.fillPattern(newPattern, m, () => throw new RuntimeException("Shouldn't need to create nodes"))

      def recursive(edgesInGraph: Set[HyperEdge[HyperTermId, HyperTermIdentifier]], root: HyperTermId, fallTo: Programs): Iterator[Term] = {
        val hyperTermToEdge = mutable.HashMultiMap(edgesInGraph.groupBy(edge => edge.target))
        val edges = hyperTermToEdge.getOrElse(root, Iterator.empty)
        if (edges.isEmpty) fallTo.reconstruct(root)
        else edges.toIterator.filter(_.metadata.forall(_ != NonConstructableMetadata)).flatMap(edge => {
          if (edge.sources.isEmpty) Iterator(new Tree[Identifier](edge.edgeType.identifier))
          else {
            val recRes = edge.sources.map(recursive(edgesInGraph - edge, _, fallTo))
            Programs.combineSeq(recRes).map(subtrees => new Tree[Identifier](edge.edgeType.identifier, subtrees.toList))
          }
        })
      }

      recursive(fullPattern, hyperTermId, this)
    })
  }

  /** Builds trees from of programs where the hyper term is the base program.
    * Disallow loops in buildings - each edges can be chose once in each branch.
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
      /** Build iterator of program's trees where their root is the current target.
        *
        * @param root The root of the programs we find
        * @return Iterator with all the programs of root.
        */
      def recursive(edgesInGraph: Set[HyperEdge[HyperTermId, HyperTermIdentifier]], root: HyperTermId): Iterator[Term] = {
        val hyperTermToEdge = mutable.HashMultiMap(edgesInGraph.groupBy(edge => edge.target))
        val edges = hyperTermToEdge.getOrElse(root, Iterator.empty)
        edges.toIterator.filter(_.metadata.forall(_ != NonConstructableMetadata)).flatMap(edge => {
          if (edge.sources.isEmpty) Iterator(new Tree[Identifier](edge.edgeType.identifier))
          else {
            val recRes = edge.sources.map(recursive(edgesInGraph - edge, _))
            Programs.combineSeq(recRes).map(subtrees => new Tree[Identifier](edge.edgeType.identifier, subtrees.toList))
          }
        })
      }

      recursive(hyperGraph.edges, hyperTermId)
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

  def empty: Programs = Programs(VersionedHyperGraph.empty[HyperTermId, HyperTermIdentifier])

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
      case Language.andCondBuilderId =>
        val res = mergeEdgesRoots(targetToSubedges)
        target = res.head._1
        res.flatMap(_._2)
      case Language.trueCondBuilderId =>
        val precondRoot = targetToSubedges.head._1
        target = targetToSubedges.last._1
        Set(
          HyperEdge(precondRoot, identToEdge(Language.trueId), List.empty, EmptyMetadata)
        )
      case Language.typeBuilderId =>
        val t = nodeCreator()
        Set(
          HyperEdge(target, identToEdge(function), targetToSubedges.map(_._1), EmptyMetadata),
          HyperEdge(t, identToEdge(Language.typeId), Seq(target), EmptyMetadata),
          HyperEdge(t, identToEdge(Language.trueId), Seq.empty, EmptyMetadata)
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

  def destructWithRoot(tree: Term, maxId: HyperTermId = HyperTermId(0)): (RewriteSearchState.HyperGraph, HyperTermId) = {
    logger.trace("Destruct a program")

    def knownTerms(t: Term): Option[HyperTermId] = None

    val hyperTermIdCreator = {
      val creator = Stream.from(maxId.id + 1).toIterator.map(HyperTermId)
      () => creator.next
    }

    val hyperEdges = innerDestruct(tree, hyperTermIdCreator, HyperTermIdentifier, knownTerms)
    (VersionedHyperGraph(hyperEdges._2.toSeq:_*), hyperEdges._1)
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

    // A specific case is where we have a pattern of type ?x -> x
    // This special case would regularly result in an empty pattern while in destruct it would result with a single edge.
    // To solve this we will create a special edge to match all the graph
    trees.map(tree => {
      if (knownTerms(tree).nonEmpty) {
        val target = knownTerms(tree).get
        val edgeType = holeCreator()
        (target, Set(HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](target, ReferenceTerm[HyperTermIdentifier](edgeType.id), Seq(RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()).get), EmptyMetadata)))
      }
      else innerDestruct[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](tree, holeCreator, edgeCreator, knownTerms)
    })
  }

  def destructPattern(tree: Term): HyperPattern = {
    destructPatternsWithRoots(Seq(tree)).head._1
  }

  def destructPatterns(trees: Seq[Term], mergeRoots: Boolean = true): Seq[HyperPattern] = {
    destructPatternsWithRoots(trees, mergeRoots).map(_._1)
  }

  private def mergeEdgesRoots[Node, EdgeType](edges: Seq[(Node, Set[HyperEdge[Node, EdgeType]])]): Seq[(Node, Set[HyperEdge[Node, EdgeType]])] = {
    val mainRoot = edges.head._1
    val roots = edges.map(_._1).toSet
    def switcher(templateTerm: Node) = if (roots.contains(templateTerm)) mainRoot else templateTerm
    edges.map(es => (mainRoot, es._2.map(e => e.copy(target = switcher(e.target), sources = e.sources.map(switcher)))))
  }

  def destructPatternsWithRoots(trees: Seq[Term], mergeRoots: Boolean = true): Seq[(HyperPattern, TemplateTerm[HyperTermId])] = {
    val edges = innerDestructPattern(trees)
    val modifiedEdges: Seq[(TemplateTerm[HyperTermId], Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]])] = {
      if (mergeRoots) mergeEdgesRoots(edges)
      else edges
    }
    modifiedEdges.map(es => (HyperGraphManyWithOrderToOne(es._2.toSeq: _*), es._1))
  }

  private val arityEdges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val builtinToHyperTermId = Language.arity.keys.zip(Stream from 0 map HyperTermId).toMap
    val builtinEdges = builtinToHyperTermId.map(kv => HyperEdge(kv._2, HyperTermIdentifier(new Identifier(kv._1)), Seq.empty, EmptyMetadata))
    builtinEdges.toSet ++ Language.arity.map(kv => HyperEdge(builtinToHyperTermId("⊤"), HyperTermIdentifier(new Identifier(s"arity${kv._2}")), Seq(builtinToHyperTermId(kv._1)), EmptyMetadata))
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
      case head +: tail =>
        var recRes = combineSeq(tail)
        for (t1 <- head;
             (newRecRes, iseqt) = recRes.duplicate;
             seqt <- iseqt) yield {
          recRes = newRecRes
          t1 +: seqt
        }
    }
  }

  def termToString(term: Term): String = {
    val lambdaDefinitions = term.nodes.map(n => n.root.kind + " name:" + n.root.literal.toString).filter(_.startsWith("Lambda Definition")).toSet

    def helper(term: Term): String = {
      val allBuiltinBool = Language.builtinAndOps ++ Language.builtinOrOps ++ Language.builtinBooleanOps ++ Language.builtinCondBuilders ++ Language.builtinDefinitions ++ Language.builtinHighLevel ++ Language.builtinSetArithOps ++ Language.builtinSetBuildingOps ++ Language.builtinIFFOps :+ Language.tacticId
      term.root match {
        case Language.annotationId => helper(term.subtrees.head)
        case Language.matchId => helper(term.subtrees(0)) + " match " + term.subtrees.tail.map(helper).mkString(" / ")
        case r if allBuiltinBool.contains(r.literal) && term.subtrees.length == 2 => Seq(helper(term.subtrees(0)), term.root.toString(), helper(term.subtrees(1))).mkString(" ")
        case _ => term.subtrees match {
          case Nil => term.root.toString()
          case list => term.root.toString() + "(" + list.map(helper).mkString(", ") + ")"
        }
      }
    }

    (lambdaDefinitions + helper(term)).mkString("\n")
  }
}