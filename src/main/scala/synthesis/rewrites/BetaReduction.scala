//package synthesis.rewrites
//
//import com.typesafe.scalalogging.LazyLogging
//import language.Language
//import structures._
//import structures.immutable.HyperGraphManyWithOrderToOne
//import syntax.Identifier
//import synthesis.{HyperTermId, HyperTermIdentifier}
//import synthesis.rewrites.RewriteRule.{SubHyperEdgePattern, SubHyperGraphPattern}
//import synthesis.search.Operator
//import rewrites._
//
//object BetaReduction extends Operator[RewriteSearchState] with LazyLogging {
//  private def holeCreator(i: Int): Hole[HyperTermId, Int] = Hole(i)
//
//  private val pattern: HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
//    // Hole 0 is the subgraph we are working on
//    // Hole 1 is the lambda and Hole 3 is the param to replace and Hole 4 is the lambda body
//    // Hole 2 is the replacement to Hole 3
//    // We want to get Hole4[Hole3 / Hole2]
//    val baseEdge = patternEdgeCreator(holeCreator(0), Language.applyId, Seq(holeCreator(1), holeCreator(2)))
//    val lambdaEdge = patternEdgeCreator(holeCreator(1), Language.lambdaId, Seq(holeCreator(3), holeCreator(4)))
//    val firstParamEdge = patternEdgeCreator(holeCreator(1), Language.lambdaId, Seq(holeCreator(3), holeCreator(4)))
//    HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](Seq(baseEdge, lambdaEdge): _*)
//  }
//
//  override def apply(state: RewriteSearchState): RewriteSearchState = {
//    val maps = state.graph.findSubgraph(pattern)
//    val newEdges = for ((idMap, _) <- maps) yield {
//      val
//    }
//  }
//}
