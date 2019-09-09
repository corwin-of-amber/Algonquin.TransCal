package synthesis.actions.operators

import structures._
import structures.immutable.HyperGraph
import synthesis._
import synthesis.actions.ActionSearchState
import synthesis.complexity.{AddComplexity, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import transcallang.{AnnotatedTree, Identifier, Language}

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

  private def findUntouchedTerms(hyperGraph: ActionSearchState.HyperGraph): RewriteSearchState.HyperGraph = {
    var filledHyperGraph = mutable.CompactHyperGraph.empty ++ hyperGraph
    for(
      timeComplexTrue <- hyperGraph.find(_.edgeType.identifier == Language.timeComplexTrueId).map(_.target);
      (idMap, _) <- hyperGraph.findSubgraph[Int](hyperPattern)
    ) if (!hyperGraph.exists(edge => edge.target == timeComplexTrue && edge.edgeType.identifier == Language.timeComplexId && edge.sources.head == idMap(0))) {
      val newNode = HyperTermId(filledHyperGraph.nodes.map(_.id).max + 1)
      val bridgeEdge = HyperEdge(timeComplexTrue, HyperTermIdentifier(Language.timeComplexId), Seq(idMap(0), newNode), EmptyMetadata)
      val timeComplexFunctionEdge = HyperEdge(newNode, HyperTermIdentifier(timeComplexFunction), idMap.filterKeys(_ > 0).toList.sortWith(_._1 < _._1).map(_._2), EmptyMetadata)
      filledHyperGraph ++= Seq(bridgeEdge, timeComplexFunctionEdge)
    }
    filledHyperGraph
  }

  private def populateEdges(hyperGraph: RewriteSearchState.HyperGraph): RewriteSearchState.HyperGraph = {
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

  private def findEquive(populated: RewriteSearchState.HyperGraph): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val populatedPrograms = Programs(populated)
    def contains(big: AnnotatedTree, small: AnnotatedTree): Boolean = (big == small) || big.subtrees.exists(contains(_, small))
    val nodeCreator = Stream.from(populated.nodes.map(_.id).max).map(HyperTermId).iterator
    val r = for ((idMaps, _) <- populated.findSubgraph[Int](hyperPattern1) if idMaps(2) != idMaps(3)) yield {
        val trueTC = idMaps(0)
        val functionNode = idMaps(1)
        //      val functionTC = idMaps(2)
        val fullTC = idMaps(3)
        val functionArguments = idMaps.filterKeys(_ >= 4).toList.sortBy(_._1).map(_._2)
        val ttt = functionArguments.map(functionArguments => (functionArguments, populatedPrograms.reconstruct(functionArguments)))
        val fullTimeComplexes = populatedPrograms.reconstructTimeComplex(fullTC).toList

        fullTimeComplexes.flatMap {
          case ConstantComplexity(_) =>
            Seq(
              HyperEdge(trueTC, HyperTermIdentifier(Language.timeComplexId), Seq(functionNode, fullTC), EmptyMetadata),
            )
          case AddComplexity(complexities) =>
            val nonConstants = complexities.collect { case ContainerComplexity(tree) => tree }.filter(_.root == timeComplexFunction)
            nonConstants match {
              case Seq(oneCall) =>
                val arguments = oneCall.subtrees

                println("arguments " + arguments)
                val filtered = ttt.zip(arguments).map(t => (t._1._1, t._1._2.filter(a => contains(a, t._2)).toSeq)).filter(_._2.nonEmpty)
                println("filtered " + filtered)
                filtered match {
                  case Seq(one) =>
                    println(one)
                    val newTC = nodeCreator.next()
                    Seq(
                      HyperEdge(trueTC, HyperTermIdentifier(Language.timeComplexId), Seq(functionNode, newTC), EmptyMetadata),
                      HyperEdge(newTC, HyperTermIdentifier(Identifier("len")), Seq(one._1), EmptyMetadata)
                    )
                }
            }
        }
      }
    r.flatten
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val filledHyperGraph = findUntouchedTerms(state.programs.hyperGraph)

    val populated = populateEdges(filledHyperGraph)

    val newEdges = findEquive(populated)

    ActionSearchState(Programs(state.programs.hyperGraph ++ newEdges), state.rewriteRules)
  }
}
