package synthesis

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.immutable.CompactHyperGraph
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.complexity.{AddComplexity, Complexity, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteRule.{HyperPattern, RewriteRuleMetadata}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.util.Try


/** Programs contains all the available programs holding them for future optimized rewrites and reconstruction of them.
  *
  * @author tomer
  * @since 11/19/18
  */
class Programs(val hyperGraph: ActionSearchState.HyperGraph) extends LazyLogging {

  private implicit class HasNextIterator[T](it: Iterator[T]) {
    def nextOption: Option[T] = if (it.hasNext) Some(it.next()) else None
  }

  /** Reconstruct annotation trees of the types.
    *
    * @param hyperTermId The root to get its types' annotation trees.
    * @return All the constructed annotation trees.
    */
  private def findTypes(hyperTermId: HyperTermId): Stream[AnnotatedTree] = {
    val searchGraph: HyperPattern = CompactHyperGraph(HyperEdge(ReferenceTerm(0),
        ExplicitTerm(HyperTermIdentifier(Language.typeId)),
        Seq(ExplicitTerm(hyperTermId), Hole(1)),
        NonConstructableMetadata))
    hyperGraph.findSubgraph[Int](searchGraph).toStream.flatMap(m => reconstructAnnotationTree(m._1(1), hyperGraph))
  }

  /* --- Public --- */
  /** Builds trees from of programs where the hyper term is the base program and term conforms to pattern.
    *
    * @param hyperTermId The hyper term to build.
    * @param pattern     graph pattern to limit output terms
    * @param patternRoot pattern root node
    * @return all conforming terms
    */
  def reconstructWithPattern(hyperTermId: HyperTermId,
                             pattern: HyperPattern,
                             patternRoot: Option[TemplateTerm[HyperTermId]] = None): Stream[AnnotatedTree] = {
    val newPattern = patternRoot.map(pattern.mergeNodes(ExplicitTerm(hyperTermId), _)).getOrElse(pattern)
    val maps = hyperGraph.findSubgraph[Int](newPattern)
    maps.toStream.flatMap(m => {
      val fullPattern = generic.HyperGraph.fillPattern(newPattern, m, () =>
        throw new RuntimeException("Shouldn't need to create nodes")
      )
      recursiveReconstruct(CompactHyperGraph.empty ++ fullPattern, hyperTermId, Some(this.reconstruct))
    })
  }

  /** Build iterator of program's trees where their root is the current target.
    *
    * @param hyperGraph The hyperGraph to use for reconstruct.
    * @param root         The root of the annotation tree.
    * @param fallTo       What to do if failed
    * @return Iterator with all the constructed annotation trees.
    */
  private def recursiveReconstruct(hyperGraph: ActionSearchState.HyperGraph,
                                   root: HyperTermId,
                                   fallTo: Option[HyperTermId => Stream[AnnotatedTree]]): Stream[AnnotatedTree] = {
    val hyperTermToEdge = hyperGraph.groupBy(_.target)
    val edges = hyperTermToEdge.getOrElse(root, CompactHyperGraph.empty)
    if (fallTo.nonEmpty && edges.isEmpty) fallTo.get(root)
    else edges.toStream.filter(_.metadata.forall(_ != NonConstructableMetadata)).flatMap(edge => {
      val typ = findTypes(edge.target)
      if (edge.sources.isEmpty) {
        Stream(AnnotatedTree(edge.edgeType.identifier.copy(annotation = typ.headOption), Seq(), typ.headOption.toSeq))
      }
      else {
        val recRes = edge.sources.map(recursiveReconstruct(hyperGraph - edge, _, fallTo))
        Programs.combineSeq(recRes).map(subtrees =>
          AnnotatedTree(edge.edgeType.identifier, subtrees.toList, typ.headOption.toSeq))
      }
    })
  }

  /** Finds annotation trees with their complexity and the relative hyper term id.
    *
    * @param termRoot The expression hyper term id.
    * @param hyperGraph The hyper graph to build from.
    * @return Tuples of annotation tree, hyper term id of the complex and the complexity.
    */
  private def innerReconstructAnnotationTreeWithTimeComplex(termRoot: HyperTermId,
                                                            hyperGraph: ActionSearchState.HyperGraph)
  : Stream[(AnnotatedTree, HyperTermId, Complexity)] = {
    hyperGraph.toStream
      .filter(e => e.edgeType.identifier == Language.timeComplexId && e.sources.head == termRoot)
      .flatMap(edge => {
        reconstructAnnotationTreeWithTimeComplex(termRoot, edge.sources(1), hyperGraph).map{case (tree, complex) =>
          (tree,  edge.sources(1), complex)
        }
    })
  }

  private def reconstructTimeComplex(complexityRoot: HyperTermId,
                                     hyperGraph: ActionSearchState.HyperGraph,
                                     fallTo: Option[HyperTermId => Stream[Complexity]]): Stream[Complexity] = {
    val hyperTermToEdge = hyperGraph.groupBy(_.target)
    val edges = hyperTermToEdge.getOrElse(complexityRoot, CompactHyperGraph.empty)
    if (fallTo.nonEmpty && edges.isEmpty) fallTo.get(complexityRoot)
    else edges.toStream.filter(_.metadata.forall(_ != NonConstructableMetadata)).flatMap(edge => {
      if (edge.sources.isEmpty) {
        edge.edgeType.identifier.literal match {
          case number if Try(number.toInt).isSuccess => Stream(ConstantComplexity(number.toInt))
          case _ =>
            reconstructAnnotationTree(complexityRoot, hyperGraph).map(Programs.termToString).map(ContainerComplexity)
        }
      } else {
        val leftHyperGraph = hyperGraph - edge
        edge.edgeType.identifier.literal match {
          case "+" =>
            Programs.combineSeq(edge.sources.map(reconstructTimeComplex(_, leftHyperGraph, fallTo)))
              .map(AddComplexity(_))
          case _ =>
            reconstructAnnotationTree(complexityRoot, hyperGraph).map(Programs.termToString).map(ContainerComplexity)
        }
      }
    })
  }

  /** Finds annotation trees with their complexity and the relative hyper term id.
    *
    * @param termRoot The expression hyper term id.
    * @param complexityRoot The complexity hyper term id.
    * @param hyperGraph The hyper graph to build from.
    * @return Tuples of annotation tree and the complexity.
    */
  private def reconstructAnnotationTreeWithTimeComplex(termRoot: HyperTermId,
                                                       complexityRoot: HyperTermId,
                                                       hyperGraph: ActionSearchState.HyperGraph)
  : Stream[(AnnotatedTree, Complexity)] = {
    val bridgeEdge = hyperGraph.find(e =>
      e.edgeType.identifier == Language.timeComplexId && e.sources == Seq(termRoot, complexityRoot)).get
    val rewriteRuleOption = bridgeEdge.metadata.collectFirst {
      case RewriteRuleMetadata(rewrite, _) =>
        rewrite
    }
    if (rewriteRuleOption.nonEmpty) {
      val rewrite = rewriteRuleOption.get
      val basicConclusion = {
        val timeComplexEdge = rewrite.conclusion.find(e =>
          e.edgeType == ExplicitTerm(HyperTermIdentifier(Language.timeComplexId))).get
        rewrite.conclusion
          .mergeNodes(ExplicitTerm(termRoot), timeComplexEdge.sources.head)
          .mergeNodes(ExplicitTerm(complexityRoot), timeComplexEdge.sources(1))
      }
      RewriteRule.fillPatterns(hyperGraph, Seq(basicConclusion, rewrite.premise)).toStream.flatMap {
        case Seq(fullConclusion, fullPremise) =>
          val fullRewrite = (fullConclusion ++ fullPremise).toSeq
          val leftHyperGraph = hyperGraph - bridgeEdge

          Programs.combineSeq(fullPremise.toSeq.filter(e => e.edgeType.identifier == Language.timeComplexId)
            .map(bridgeEdgeInPremise => {
              reconstructAnnotationTreeWithTimeComplex(bridgeEdgeInPremise.sources.head,
                bridgeEdgeInPremise.sources(1),
                leftHyperGraph
              ).map(res => ((bridgeEdgeInPremise.sources.head, res._1), (bridgeEdgeInPremise.sources(1), res._2)))
            })).flatMap{combination =>
            val annotationTrees = {
              val rootsToTrees = combination.map(t => t._1).toMap
              def annotationTreeLinker(rootAnnotatedTree: HyperTermId): Stream[AnnotatedTree] = {
                Stream(rootsToTrees(rootAnnotatedTree))
              }
              recursiveReconstruct(CompactHyperGraph(fullRewrite:_*), termRoot, Some(annotationTreeLinker)).toList
            }

            val timeComplexes = {
              val rootsToComplexities = combination.map(t => t._2).toMap.mapValues(Stream(_))
              def timeComplexLinker(rootTimeComplex: HyperTermId): Stream[Complexity] = {
                if (rootsToComplexities.contains(rootTimeComplex)) {
                  rootsToComplexities(rootTimeComplex)
                } else {
                  reconstructTimeComplex(rootTimeComplex, leftHyperGraph, Some(timeComplexLinker))
                }
              }
              reconstructTimeComplex(complexityRoot, CompactHyperGraph(fullRewrite:_*), Some(timeComplexLinker)).toList
            }
            for (tree <- annotationTrees ; complex <- timeComplexes) yield {
              (tree, complex)
            }
          }
      }
    } else {
        Programs.combineSeq(Seq(reconstructAnnotationTree(bridgeEdge.sources.head, hyperGraph),
          reconstructTimeComplex(complexityRoot, hyperGraph)))
          .map { s => (s.head.asInstanceOf[AnnotatedTree], s(1).asInstanceOf[Complexity]) }
    }
  }
  /** Reconstruct annotation trees.
    *
    * @param root The root of the annotation tree.
    * @param hyperGraph The hyperGraph to use as reference.
    * @return All the constructed annotation trees.
    */
  private def reconstructAnnotationTree(root: HyperTermId, hyperGraph: ActionSearchState.HyperGraph)
  : Stream[AnnotatedTree] = recursiveReconstruct(hyperGraph, root, None)

  private def reconstructTimeComplex(complexityRoot: HyperTermId, hyperGraph: ActionSearchState.HyperGraph)
  : Stream[Complexity] = reconstructTimeComplex(complexityRoot, hyperGraph, None)

  def reconstructWithTimeComplex(termRoot: HyperTermId): Stream[(AnnotatedTree, Complexity)] = {
    innerReconstructAnnotationTreeWithTimeComplex(termRoot, hyperGraph).map{case (tree, _, complex) => (tree, complex)}
  }

  /** Builds trees from of programs where the hyper term is the base program.
    * Disallow loops in buildings - each edges can be chose once in each branch.
    *
    * @param hyperTermId The hyper term to build.
    * @return All the trees.
    */
  def reconstruct(hyperTermId: HyperTermId): Stream[AnnotatedTree] = {
    logger.trace("Reconstruct programs")

    if (!hyperGraph.nodes.contains(hyperTermId)) {
      logger.debug(f"Unknown HyperTerm - $hyperTermId")
      Stream.empty
    } else {
      recursiveReconstruct(hyperGraph, hyperTermId, None)
    }
  }

  /** Adds a new term to the programs.
    *
    * @param term The new term to add.
    * @return New programs with the term in it.
    */
  def addTerm(term: AnnotatedTree): Programs = {
    Programs(hyperGraph ++
      Programs.destruct(term, if (hyperGraph.isEmpty) HyperTermId(0) else hyperGraph.nodes.maxBy(_.id)))
  }

  def +(term: AnnotatedTree): Programs = addTerm(term)


  /* --- Object Impl. --- */

  override def toString: String = f"Programs($hyperGraph)"
}

object Programs extends LazyLogging {
  type Edge = HyperEdge[HyperTermIdentifier, HyperTermId]
  def Edge(hyperTermId: HyperTermId, hyperTermIdentifier: HyperTermIdentifier, sources: Seq[HyperTermId])
  : HyperEdge[HyperTermId, HyperTermIdentifier] = HyperEdge(hyperTermId, hyperTermIdentifier, sources, EmptyMetadata)

  /* --- Public --- */
  object NonConstructableMetadata extends Metadata {
    override protected def toStr: String = "NonConstructable"
  }

  def empty: Programs = Programs(CompactHyperGraph.empty[HyperTermId, HyperTermIdentifier])

  def apply(hyperGraph: ActionSearchState.HyperGraph): Programs = new Programs(hyperGraph)

  def apply(hyperGraph: RewriteSearchState.HyperGraph): Programs = new Programs(CompactHyperGraph(hyperGraph.toSeq: _*))

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
                                            identToEdge: Identifier => EdgeType)
  : (Node, Set[HyperEdge[Node, EdgeType]]) = {
    val (function, args) = flattenApply(tree)
    // Skipping annotations, shouldn't be part of the graph, at least for now
    if (function.literal == "Annotation") return innerDestruct(tree.subtrees(0), nodeCreator, identToEdge)

    val targetToSubedges = args.map(subtree => innerDestruct(subtree, nodeCreator, identToEdge))
    val subHyperEdges = targetToSubedges.flatMap(_._2).toSet

    val (target, newHyperEdges) = function match {
      case Language.limitedAndCondBuilderId =>
        (targetToSubedges.head._1, targetToSubedges.flatMap(_._2))
      case Language.andCondBuilderId =>
        val res = mergeEdgesRoots(targetToSubedges)
        (res.head._1,
        res.flatMap(_._2)
        )
      case Language.trueCondBuilderId =>
        val precondRoot = targetToSubedges.head._1
        (targetToSubedges.last._1,
          subHyperEdges +
            HyperEdge(precondRoot, identToEdge(Language.trueId.copy(annotation = None)), List.empty, EmptyMetadata)
        )
      case _ =>
        val target = nodeCreator.next()
        (
          target,
          subHyperEdges +
            HyperEdge(target, identToEdge(function.copy(annotation = None)), targetToSubedges.map(_._1), EmptyMetadata)
        )
    }
    val annotationEdges = function.annotation match {
      case Some(annotatedTree) =>
        val (targetType, hyperEdgesType) = innerDestruct(annotatedTree, nodeCreator, identToEdge)
        val trueNode = nodeCreator.next()
        val functionNode = {
          val found = newHyperEdges.find(e =>
            e.sources.isEmpty && e.edgeType == identToEdge(function.copy(annotation = None)))
          if (found.nonEmpty) found.get.target else nodeCreator.next()
        }
        hyperEdgesType ++ Set(
          HyperEdge(functionNode, identToEdge(function.copy(annotation = None)), Seq.empty, EmptyMetadata),
          HyperEdge(trueNode, identToEdge(Language.typeId), Seq(functionNode, targetType), EmptyMetadata),
          HyperEdge(trueNode, identToEdge(Language.typeTrueId), Seq.empty, EmptyMetadata)
        )
      case None => Set.empty
    }

    (target, annotationEdges ++ newHyperEdges)
  }

  /** Create hyper graph from ast. Removes annotations. Root is always max HyperTermId.
    *
    * @param tree  - program to be transformed into hypergraph
    * @param maxId - Max id so no ids will cross
    * @return
    */
  def destruct(tree: AnnotatedTree, maxId: HyperTermId = HyperTermId(0)): ActionSearchState.HyperGraph = {
    destructWithRoot(tree, maxId)._1
  }

  def destructWithRoot(tree: AnnotatedTree, maxId: HyperTermId = HyperTermId(0))
  : (ActionSearchState.HyperGraph, HyperTermId) = {
    logger.trace("Destruct a program")

    val hyperEdges = innerDestruct(tree, Stream.from(maxId.id + 1).iterator.map(HyperTermId), HyperTermIdentifier)
    (CompactHyperGraph(hyperEdges._2.toSeq: _*), hyperEdges._1)
  }

  def destructPattern(tree: AnnotatedTree): HyperPattern = {
    destructPatternsWithRoots(Seq(tree)).head._1
  }

  def destructPatterns(trees: Seq[AnnotatedTree], mergeRoots: Boolean = true): Seq[HyperPattern] = {
    destructPatternsWithRoots(trees, mergeRoots).map(_._1)
  }

  private def mergeEdgesRoots[Node, EdgeType](edges: Seq[(Node, Set[HyperEdge[Node, EdgeType]])])
  : Seq[(Node, Set[HyperEdge[Node, EdgeType]])] = {
    val mainRoot = edges.minBy(_._2.size)._1
    val roots = edges.map(_._1).toSet

    def switcher(templateTerm: Node) = if (roots.contains(templateTerm)) mainRoot else templateTerm

    edges.map(es => (mainRoot, es._2.map(e => e.copy(target = switcher(e.target), sources = e.sources.map(switcher)))))
  }

  def destructPatternsWithRoots(trees: Seq[AnnotatedTree], mergeRoots: Boolean = true)
  : Seq[(HyperPattern, TemplateTerm[HyperTermId])] = {

    // ------------------ Create Edges -----------------------
    def edgeCreator(i: Identifier): TemplateTerm[HyperTermIdentifier] = ExplicitTerm(HyperTermIdentifier(i))

    val holeCreator = Stream.from(0).iterator.map(ReferenceTerm[HyperTermId])
    val vars = trees.flatMap(t => t.leaves.filter(_.root.literal.startsWith("?"))).map(t =>
      (t.root.copy(literal = t.root.literal.drop(1)), t.root)).toMap

    def holeFuncToApply(tree: AnnotatedTree): AnnotatedTree = {
      if (tree.subtrees.nonEmpty && vars.values.exists(_ == tree.root))
        AnnotatedTree.withoutAnnotations(Language.applyId,
          AnnotatedTree.identifierOnly(tree.root) +: tree.subtrees.map(holeFuncToApply))
      else
        tree.copy(subtrees = tree.subtrees.map(holeFuncToApply))
    }

    val updatedTrees = {
      val holeIt = Stream.from(0).iterator.map(i => Identifier(s"?_autohole$i"))
      trees.map(t => t.map({
        case Language.holeId => holeIt.next()
        case i if vars.contains(i) => vars(i)
        case i => i
      })).map(holeFuncToApply)
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
        val inner =
          innerDestruct[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](tree, holeCreator, edgeCreator)
        (inner._1, inner._2)
      }
    })

    // ------------------ To Compact Graph -----------------------
    def anchorCreator(i: Int) = ExplicitTerm(HyperTermIdentifier(Identifier(s"Pattern anchor for $i")))

    // Add anchors on roots to return the right root later
    val anchoredGraphs = edges.zipWithIndex.map({case ((target, graphEdges), i) =>
      val anchorEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
        target, anchorCreator(i), Seq.empty, NonConstructableMetadata)
      CompactHyperGraph(graphEdges.toSeq :+ anchorEdge: _*)
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
      if (mergeRoots) afterCompaction
        .map({case (graph, target) => (graph.mergeNodes(afterCompaction.head._2, target), afterCompaction.head._2)})
      else afterCompaction
    }

    for ((graph, target) <- results) {
      assert(graph.nodes.contains(target))
    }
    results
  }

  /** Iterator which combines sequence of iterators (return all combinations of their results).
    *
    * @param iterators All the iterators to combine.
    * @tparam T The return type.
    */
  def combineSeq[T](iterators: Seq[Stream[T]]): Stream[Seq[T]] = {
    iterators match {
      case Nil => Stream.empty
      case head +: Nil => head.map(Seq(_))
      case head +: tail =>
        var recRes = combineSeq(tail)
        for (t1 <- head;
             seqt <- recRes) yield {
          t1 +: seqt
        }
    }
  }

  def termToString(term: AnnotatedTree): String = {
    val lambdaDefinitions = term.nodes.map(n => " name:" + n.root.literal)
      .filter(_.startsWith("Lambda Definition")).toSet

    def helper(term: AnnotatedTree): String = {
      val allBuiltinBool = Language.builtinBooleanOps ++ Language.builtinCondBuilders ++ Language.builtinDefinitions ++
        Language.builtinHighLevel ++ Language.builtinSetArithOps ++ Language.builtinSetBuildingOps :+
        Language.tacticId :+ Language.andId :+ Language.orId
      term.root match {
        case Language.annotationId => helper(term.subtrees.head)
        case Language.matchId => helper(term.subtrees(0)) + " match " + term.subtrees.tail.map(helper).mkString(" / ")
        case Language.setId => "{" + term.subtrees.map(helper).mkString(", ") + "}"
        case r if allBuiltinBool.contains(r) && term.subtrees.length == 2 =>
          Seq(helper(term.subtrees(0)), term.root.literal, helper(term.subtrees(1))).mkString(" ")
        case _ => term.subtrees match {
          case Nil => term.root.literal
          case list => term.root.literal + "(" + list.map(helper).mkString(", ") + ")"
        }
      }
    }

    (lambdaDefinitions + helper(term)).mkString("\n")
  }
}