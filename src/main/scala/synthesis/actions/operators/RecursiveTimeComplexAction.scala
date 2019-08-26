package synthesis.actions.operators

import structures._
import structures.immutable.HyperGraph
import structures.mutable.VersionedHyperGraph
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis._
import transcallang.{Identifier, Language}

/** Calculate time complex of recursive terms.
  *
  */
class RecursiveTimeComplexAction(function: Identifier, arguments: Int) extends Action {
  assume(arguments > 0)

  /** The function edge regex to look for. */
  private val functionEdgeRegex = HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(function)), Stream.from(1).map(ReferenceTerm(_)).take(arguments), EmptyMetadata)

  /** The function pattern. */
  private val hyperPattern: HyperPattern = HyperGraph(functionEdgeRegex)

  /** The function hole. */
  private val timeComplexFunction = Identifier("f")

  /** New function time complex matches. */
  private val hyperPattern1: HyperPattern = {
    val timeComplexTrueNode = ReferenceTerm(0)
    val functionNode = ReferenceTerm(1)
    val tcFNode = ReferenceTerm(2)
    val tcFullNode = ReferenceTerm(3)
    val argumentsRepetition = Stream.from(4).map(ReferenceTerm(_)).take(arguments)
    HyperGraph(
      HyperEdge(timeComplexTrueNode, ExplicitTerm(HyperTermIdentifier(Language.timeComplexTrueId)), Seq.empty, EmptyMetadata),
      HyperEdge(timeComplexTrueNode, ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(functionNode, tcFNode), EmptyMetadata),
      HyperEdge(timeComplexTrueNode, ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(functionNode, tcFullNode), EmptyMetadata),
      HyperEdge(functionNode, ExplicitTerm(HyperTermIdentifier(function)), argumentsRepetition, EmptyMetadata),
      HyperEdge(tcFNode, ExplicitTerm(HyperTermIdentifier(timeComplexFunction)), argumentsRepetition, EmptyMetadata),
    )
  }

  private def findUntouchedTerms(hyperGraph: ActionSearchState.HyperGraph): VersionedHyperGraph[HyperTermId, HyperTermIdentifier] = {
    var filledHyperGraph = mutable.VersionedHyperGraph.empty ++ hyperGraph
    for(
      timeComplexTrue <- hyperGraph.find(_.edgeType.identifier == Language.timeComplexTrueId).map(_.target);
      (idMap, _) <- hyperGraph.findSubgraph[Int](hyperPattern)
    ) if (!hyperGraph.exists(edge => edge.target == timeComplexTrue && edge.edgeType.identifier == Language.timeComplexId && edge.sources.head == idMap(0))) {
      val newNode = HyperTermId(filledHyperGraph.nodes.map(_.id).max + 1)
      val bridgeEdge = HyperEdge(timeComplexTrue, HyperTermIdentifier(Language.timeComplexId), Seq(idMap(0), newNode), NonConstructableMetadata)
      val timeComplexFunctionEdge = HyperEdge(newNode, HyperTermIdentifier(timeComplexFunction), idMap.filterKeys(_ > 0).toList.sortWith(_._1 < _._1).map(_._2), NonConstructableMetadata)
      filledHyperGraph ++= Seq(bridgeEdge, timeComplexFunctionEdge)
    }
    filledHyperGraph
  }

  private def populateEdges(hyperGraph: VersionedHyperGraph[HyperTermId, HyperTermIdentifier]): VersionedHyperGraph[HyperTermId, HyperTermIdentifier] = {
    var tempHyperGraph = hyperGraph
    var lastSize = 0
    var size = tempHyperGraph.size
    do {
      for (rewriteRule <- SpaceComplexRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules) {
        tempHyperGraph = rewriteRule.apply(RewriteSearchState(tempHyperGraph)).graph
      }
      lastSize = size
      size = tempHyperGraph.size
    } while (size !=lastSize)
    tempHyperGraph
  }

  def findEquive(populated: VersionedHyperGraph[HyperTermId, HyperTermIdentifier]): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    for ((idMaps, _) <- populated.findSubgraph[Int](hyperPattern1)) if (idMaps(2) != idMaps(3)) {
      println(idMaps)
    }
    Set.empty
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val filledHyperGraph = findUntouchedTerms(state.programs.hyperGraph)

    val populated = populateEdges(filledHyperGraph)

    val newEdges = findEquive(populated)

    ActionSearchState(Programs(state.programs.hyperGraph ++ newEdges), state.rewriteRules)
  }
}
