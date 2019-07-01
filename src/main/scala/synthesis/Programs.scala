package synthesis

import com.typesafe.scalalogging.LazyLogging
import transcallang.{Identifier, Language}
import structures.immutable.VersionedHyperGraph
import structures._
import transcallang.AnnotatedTree
import synthesis.Programs.NonConstructableMetadata
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}

import scala.collection.mutable


/** Programs contains all the available programs holding them for future optimized rewrites and reconstruction of them.
  *
  * @author tomer
  * @since 11/19/18
  */
class Programs(val hyperGraph: HyperGraph) extends LazyLogging {

  implicit class HasNextIterator[T](it: Iterator[T]) {
    def nextOption: Option[T] = if (it.hasNext) Some(it.next()) else None
  }

  def findTypes(hyperTermId: HyperTermId): Iterator[AnnotatedTree] = {
    val searchGraph: VersionedHyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
      VersionedHyperGraph(HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.typeId)), Seq(ExplicitTerm(hyperTermId), Hole(1)), NonConstructableMetadata))
    hyperGraph.findSubgraph[Int](searchGraph).flatMap(m => reconstruct(m._1(1))).toIterator
  }


  /* --- Public --- */
  /** Builds trees from of programs where the hyper term is the base program and term conforms to pattern.
    *
    * @param hyperTermId The hyper term to build.
    * @param pattern     graph pattern to limit output terms
    * @param patternRoot pattern root node
    * @return all conforming terms
    */
  def reconstructWithPattern(hyperTermId: HyperTermId, pattern: HyperPattern, patternRoot: Option[TemplateTerm[HyperTermId]] = None): Iterator[AnnotatedTree] = {
    val newPattern = patternRoot.map(pattern.mergeNodes(ExplicitTerm(hyperTermId), _)).getOrElse(pattern)
    val maps = hyperGraph.findSubgraph[Int](newPattern)
    maps.toIterator.flatMap(m => {
      val fullPattern = HyperGraphManyWithOrderToOneLike.fillPattern(newPattern, m, () => throw new RuntimeException("Shouldn't need to create nodes"))
      recursiveReconstruct(fullPattern, hyperTermId, Some(this))
    })
  }

  /** Build iterator of program's trees where their root is the current target.
    *
    * @param edgesInGraph All edges to use for reconstruct
    * @param root         The root of the programs we find
    * @param fallTo       What to do if failed
    * @return Iterator with all the programs of root.
    */
  private def recursiveReconstruct(edgesInGraph: Set[HyperEdge[HyperTermId, HyperTermIdentifier]], root: HyperTermId, fallTo: Option[Programs]): Iterator[AnnotatedTree] = {
    val hyperTermToEdge = mutable.HashMultiMap(edgesInGraph.groupBy(edge => edge.target))
    val edges = hyperTermToEdge.getOrElse(root, Iterator.empty)
    if (fallTo.nonEmpty && edges.isEmpty) fallTo.get.reconstruct(root)
    else edges.toIterator.filter(_.metadata.forall(_ != NonConstructableMetadata)).flatMap(edge => {
      val typ = findTypes(edge.target)
      if (edge.sources.isEmpty) {
        Iterator(AnnotatedTree.identifierOnly(edge.edgeType.identifier.copy(annotation = typ.nextOption)))
      }
      else {
        val recRes = edge.sources.map(recursiveReconstruct(edgesInGraph - edge, _, fallTo))
        Programs.combineSeq(recRes).map(subtrees => AnnotatedTree(edge.edgeType.identifier, subtrees.toList, typ.nextOption.toSeq))
      }
    })
  }

  /** Builds trees from of programs where the hyper term is the base program.
    * Disallow loops in buildings - each edges can be chose once in each branch.
    *
    * @param hyperTermId The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTermId: HyperTermId): Iterator[AnnotatedTree] = {
    logger.trace("Reconstruct programs")

    if (!hyperGraph.nodes.contains(hyperTermId)) {
      logger.debug(f"Unknown HyperTerm - $hyperTermId")
      Iterator.empty
    } else {
      recursiveReconstruct(hyperGraph.edges, hyperTermId, None)
    }
  }

  /** Adds a new term to the programs.
    *
    * @param term The new term to add.
    * @return New programs with the term in it.
    */
  def addTerm(term: AnnotatedTree): Programs = {
    Programs(hyperGraph ++ Programs.destruct(term, if (hyperGraph.nodes.isEmpty) HyperTermId(0) else hyperGraph.nodes.maxBy(_.id)))
  }

  def +(term: AnnotatedTree): Programs = addTerm(term)


  /* --- Object Impl. --- */

  override def toString: String = f"Programs($hyperGraph)"
}

object Programs extends LazyLogging {

  /* --- Public --- */
  object NonConstructableMetadata extends Metadata {
    override protected def toStr: String = "NonConstructable"
  }

  def empty: Programs = Programs(VersionedHyperGraph.empty[HyperTermId, HyperTermIdentifier])

  def apply(hyperGraph: RewriteSearchState.HyperGraph): Programs = new Programs(hyperGraph)

  def apply(tree: AnnotatedTree): Programs = Programs(Programs.destruct(tree))

  private def flattenApply(term: AnnotatedTree): (Identifier, Seq[AnnotatedTree]) = {
    if (term.root == Language.applyId && term.subtrees.head.root == Language.applyId) {
      val (fun, args) = flattenApply(term.subtrees.head)
      (fun, args ++ term.subtrees.tail)
    }
    else (term.root, term.subtrees)
  }

  private def innerDestruct[Node, EdgeType](tree: AnnotatedTree,
                                            nodeCreator: Iterator[Node],
                                            identToEdge: Identifier => EdgeType): (Node, Set[HyperEdge[Node, EdgeType]]) = {
    val (function, args) = flattenApply(tree)
    // Skipping annotations, shouldn't be part of the graph, at least for now
    if (function.literal == "Annotation") return innerDestruct(tree.subtrees(0), nodeCreator, identToEdge)

    val targetToSubedges = args.map(subtree => innerDestruct(subtree, nodeCreator, identToEdge))
    val subHyperEdges = targetToSubedges.flatMap(_._2).toSet
    var target = nodeCreator.next()

    val newHyperEdges = function match {
      case Language.`limitedAndCondBuilderId` =>
        target = targetToSubedges.head._1
        targetToSubedges.flatMap(_._2)
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
      case _ => Set(HyperEdge(target, identToEdge(function.copy(annotation = None)), targetToSubedges.map(_._1), EmptyMetadata))
    }
    val annotationEdges = function.annotation match {
      case Some(annotatedTree) =>
        val (targetType, hyperEdgesType) = innerDestruct(annotatedTree, nodeCreator, identToEdge)
        val trueNode = nodeCreator.next()
        val functionNode = nodeCreator.next()
        hyperEdgesType ++ Set(
          HyperEdge(functionNode, identToEdge(function.copy(annotation = None)), Seq.empty, EmptyMetadata),
          HyperEdge(trueNode, identToEdge(Language.typeId), Seq(functionNode, targetType), EmptyMetadata),
          HyperEdge(trueNode, identToEdge(Language.trueId), Seq.empty, EmptyMetadata)
        )
      case None => Set.empty
    }

    (target, subHyperEdges ++ newHyperEdges ++ annotationEdges)
  }

  /** Create hyper graph from ast. Removes annotations. Root is always max HyperTermId.
    *
    * @param tree  - program to be transformed into hypergraph
    * @param maxId - Max id so no ids will cross
    * @return
    */
  def destruct(tree: AnnotatedTree, maxId: HyperTermId = HyperTermId(0)): RewriteSearchState.HyperGraph = {
    destructWithRoot(tree, maxId)._1
  }

  def destructWithRoot(tree: AnnotatedTree, maxId: HyperTermId = HyperTermId(0)): (RewriteSearchState.HyperGraph, HyperTermId) = {
    logger.trace("Destruct a program")

    val hyperEdges = innerDestruct(tree, Stream.from(maxId.id + 1).toIterator.map(HyperTermId), HyperTermIdentifier)
    (VersionedHyperGraph(hyperEdges._2.toSeq: _*), hyperEdges._1)
  }

  def destructPattern(tree: AnnotatedTree): HyperPattern = {
    destructPatternsWithRoots(Seq(tree)).head._1
  }

  def destructPatterns(trees: Seq[AnnotatedTree], mergeRoots: Boolean = true): Seq[HyperPattern] = {
    destructPatternsWithRoots(trees, mergeRoots).map(_._1)
  }

  private def mergeEdgesRoots[Node, EdgeType](edges: Seq[(Node, Set[HyperEdge[Node, EdgeType]])]): Seq[(Node, Set[HyperEdge[Node, EdgeType]])] = {
    val mainRoot = edges.head._1
    val roots = edges.map(_._1).toSet

    def switcher(templateTerm: Node) = if (roots.contains(templateTerm)) mainRoot else templateTerm

    edges.map(es => (mainRoot, es._2.map(e => e.copy(target = switcher(e.target), sources = e.sources.map(switcher)))))
  }

  def destructPatternsWithRoots(trees: Seq[AnnotatedTree], mergeRoots: Boolean = true): Seq[(HyperPattern, TemplateTerm[HyperTermId])] = {

    // ------------------ Create Edges -----------------------
    def edgeCreator(i: Identifier): TemplateTerm[HyperTermIdentifier] = ExplicitTerm(HyperTermIdentifier(i))

    val holeCreator = Stream.from(0).iterator.map(ReferenceTerm[HyperTermId])
    val vars = trees.flatMap(t => t.leaves.filter(_.root.literal.startsWith("?"))).map(t =>
      (t.root.copy(literal = t.root.literal.drop(1)), t.root)).toMap

    val updatedTrees = {
      val holeIt = Stream.from(0).iterator.map(i => Identifier(s"?_autohole$i"))
      trees.map(t => t.map({
        case Language.holeId => holeIt.next()
        case i if vars.contains(i) => vars(i)
        case i => i
      }))
    }

    val varHoles = (updatedTrees
      .flatMap(_.nodes.filter(_.root.literal.startsWith("?_autohole")).map(_.root)) ++ vars.values)
      .map(v => (v, holeCreator.next())).toMap

    val edges = updatedTrees.map(tree => {
      if (tree.size == 1 && tree.root.literal.startsWith("?") && tree.root.annotation.isEmpty) {
        // Case of a single hole without edges in graph
        val target = varHoles(tree.root)
        val edgeType = holeCreator.next()
        (target,
          Set(HyperEdge(target, ReferenceTerm[HyperTermIdentifier](edgeType.id),
            Seq(RepetitionTerm.rep0[HyperTermId](Int.MaxValue, Ignored[HyperTermId, Int]()).get), EmptyMetadata))
        )
      }
      else {
        val inner = innerDestruct[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](tree, holeCreator, edgeCreator)
        (inner._1, inner._2)
      }
    })

    // ------------------ To Compact Graph -----------------------
    def anchorCreator(i: Int) = ExplicitTerm(HyperTermIdentifier(Identifier(s"Pattern anchor for $i")))

    // Add anchors on roots to return the right root later
    val anchoredGraphs = edges.zipWithIndex.map({case ((target, graphEdges), i) =>
      val anchorEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
        target, anchorCreator(i), Seq.empty, NonConstructableMetadata)
      VersionedHyperGraph(graphEdges.toSeq :+ anchorEdge: _*)
    })

    val mergingVarHoles = anchoredGraphs.map(g => varHoles.foldLeft(g)({ case (graph, (identifier, hole)) =>
      val holeEdges = graph.findEdges(ExplicitTerm(HyperTermIdentifier(identifier.copy(annotation = None))))
      val merged = holeEdges.foldLeft(graph)((g, e) => g.mergeNodes(hole, e.target))
      merged.filter(_.edgeType match {
        case ExplicitTerm(HyperTermIdentifier(i)) if i == identifier.copy(annotation = None) => false
        case _ => true
      })
    }))

    val afterCompaction = mergingVarHoles.zipWithIndex.map({ case (graph, i) =>
      (graph.filter(e => e.edgeType != anchorCreator(i)), graph.findEdges(anchorCreator(i)).head.target)})

    val results = {
      if (mergeRoots) afterCompaction.map({case (graph, target) => (graph.mergeNodes(afterCompaction.head._2, target), afterCompaction.head._2)})
      else afterCompaction
    }

    for ((graph, target) <- results) {
      assert(graph.nodes.contains(target))
    }
    results
  }

  private val arityEdges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val builtinToHyperTermId = Language.arity.keys.zip(Stream from 0 map HyperTermId).toMap
    val builtinEdges = builtinToHyperTermId.map(kv => HyperEdge(kv._2, HyperTermIdentifier(Identifier(kv._1)), Seq.empty, EmptyMetadata))
    builtinEdges.toSet ++ Language.arity.map(kv => HyperEdge(builtinToHyperTermId("⊤"), HyperTermIdentifier(Identifier(s"arity${kv._2}")), Seq(builtinToHyperTermId(kv._1)), EmptyMetadata))
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

  def termToString(term: AnnotatedTree): String = {
    val lambdaDefinitions = term.nodes.map(n => " name:" + n.root.literal).filter(_.startsWith("Lambda Definition")).toSet

    def helper(term: AnnotatedTree): String = {
      val allBuiltinBool = Language.builtinBooleanOps ++ Language.builtinCondBuilders ++ Language.builtinDefinitions ++ Language.builtinHighLevel ++ Language.builtinSetArithOps ++ Language.builtinSetBuildingOps :+ Language.tacticId :+ Language.andId :+ Language.orId
      term.root match {
        case Language.annotationId => helper(term.subtrees.head)
        case Language.matchId => helper(term.subtrees(0)) + " match " + term.subtrees.tail.map(helper).mkString(" / ")
        case Language.setId => "{" + term.subtrees.map(helper).mkString(", ") + "}"
        case r if allBuiltinBool.contains(r) && term.subtrees.length == 2 => Seq(helper(term.subtrees(0)), term.root.literal, helper(term.subtrees(1))).mkString(" ")
        case _ => term.subtrees match {
          case Nil => term.root.literal
          case list => term.root.literal + "(" + list.map(helper).mkString(", ") + ")"
        }
      }
    }

    (lambdaDefinitions + helper(term)).mkString("\n")
  }
}