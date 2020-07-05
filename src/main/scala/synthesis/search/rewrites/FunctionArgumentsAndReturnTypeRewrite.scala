package synthesis.search.rewrites

import structures._
import synthesis.search.rewrites.Template.TemplateTerm
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Language

/** This rewrite rule finds functions return types by looking on their types.
  */
object FunctionArgumentsAndReturnTypeRewrite extends RewriteRule {

  object ApplyTypeMetadata extends Metadata {
    override protected def toStr: String = "ApplyTypeMetadata"
  }
  object ArgumentsTypeMetadata extends Metadata {
    override protected def toStr: String = "ArgumentsTypeMetadata"
  }

  // Used holes
  private val trueIdHole = Hole(0)
  private val functionIdHole = Hole(1)
  private val functionTypeIdHole = Hole(2)
  private val functionIdentifierHole = Hole(3)
  private val functionReturnTypeHole = Hole(4)
  private val functionApplicationHole = Hole(5)
  private val LARGEST_STATIC_HOLE = 6
  private val argumentTypesHoles = Repetition.rep1(Int.MaxValue, Stream.from(LARGEST_STATIC_HOLE).filter(_ % 2 == 0).map(Hole(_))).get
  private val argumentHoles = Repetition.rep1(Int.MaxValue, Stream.from(LARGEST_STATIC_HOLE).filter(_ % 2 == 1).map(Hole(_))).get

  // Used edges
  private val trueEdge = rewrites.patternEdgeCreator(trueIdHole, Language.typeTrueId, Seq())
  private val functionTypeEdge = rewrites.patternEdgeCreator(trueIdHole, Language.typeId, Seq(functionIdHole, functionTypeIdHole))
  private val functionIdEdge = rewrites.patternEdgeCreator(functionIdHole, functionIdentifierHole, Seq())

  private val functionReturnTypeEdge = rewrites.patternEdgeCreator(functionTypeIdHole, Language.mapTypeId, Seq(argumentTypesHoles, functionReturnTypeHole))
  private val functionApplicationEdge = rewrites.patternEdgeCreator(functionApplicationHole, functionIdentifierHole, Seq(argumentHoles))

  private val funcTypeGraph = generic.HyperGraph(trueEdge, functionTypeEdge, functionIdEdge, functionReturnTypeEdge, functionApplicationEdge)

  override def apply(graph: RewriteRule.HyperGraph): Unit = {
    val newFuncEdges = getNewEdges(graph, false)

    graph.++=(newFuncEdges)
  }

  private def getNewEdges(graph: RewriteRule.HyperGraph, versioned: Boolean) = {
    val newFuncEdges =
      (if (versioned) graph.findSubgraphVersioned[Int](funcTypeGraph)
      else graph.findSubgraph[Int](funcTypeGraph))
      .flatMap { case (idMap, _) =>
        val argumentsHyperEdges = for (pairs <- idMap.filterKeys(LARGEST_STATIC_HOLE <= _).groupBy(_._1 / 2).values) yield {
          val argumentType = pairs.filterKeys(_ % 2 == 0).values.head
          val argument = pairs.filterKeys(_ % 2 == 1).values.head
          HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(argument, argumentType), ArgumentsTypeMetadata)
        }
        val returnHyperEdge = HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(idMap(functionApplicationHole.id), idMap(functionReturnTypeHole.id)), ApplyTypeMetadata)
        argumentsHyperEdges.toSet + returnHyperEdge
      }
    newFuncEdges
  }

  /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
    * unless operator is existential
    *
    * @param graph state on which to run operator
    * @return (new state after update, next relevant version)
    */
  override def applyVersioned(graph: RewriteRule.HyperGraph): Unit = {
    val newFuncEdges = getNewEdges(graph, true)

    graph.++=(newFuncEdges)
  }

  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param graph     current state from which to do the initial calculations and create an operator
    * @param versioned if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  override def getStep(graph: RewriteRule.HyperGraph, versioned: Boolean): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
    val newFuncEdges = getNewEdges(graph, versioned)

    newFuncEdges.map(e => e.copy(target = Explicit(e.target), edgeType = Explicit(e.edgeType), sources = e.sources.map(Explicit(_))))
  }

  override def isExistential: Boolean = false
}
