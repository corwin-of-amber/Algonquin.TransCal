//package synthesis.rewrites
//
//import com.typesafe.scalalogging.LazyLogging
//import structures.{EmptyMetadata, Explicit, Hole}
//import structures.immutable.HyperGraphManyWithOrderToOne
//import syntax.Identifier
//import synthesis.HyperTermIdentifier
//import synthesis.rewrites.RewriteRule.{SubHyperEdgePattern, SubHyperGraphPattern}
//import synthesis.search.Operator
//
//object BetaReduction extends Operator[RewriteSearchState] with LazyLogging {
//  private val pattern: SubHyperGraphPattern = {
//
//    // Hole 0 is the subgraph we are working on
//    // Hole 1 is the lambda and Hole 3 is the param to replace and Hole 4 is the lambda body
//    // Hole 2 is the replacement to Hole 3
//    // We want to get Hole4[Hole3 / Hole2]
//    val baseEdge = new SubHyperEdgePattern(Hole(0), Explicit(HyperTermIdentifier(new Identifier("@"))), Seq(Hole(1), Hole(2)), EmptyMetadata)
//    val lambdaEdge = new SubHyperEdgePattern(Hole(1), Explicit(HyperTermIdentifier(new Identifier("->"))), Seq(Hole(3), Hole(4)), EmptyMetadata)
//    HyperGraphManyWithOrderToOne(Set())
//  }
//
//  override def apply(state: RewriteSearchState): RewriteSearchState = {
//
//  }
//}
