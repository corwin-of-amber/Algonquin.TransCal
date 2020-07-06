package synthesis.search.rewrites

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures._
import structures.generic.HyperGraph.Match
import structures.mutable.CompactHyperGraph
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.rewrites.operators._
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier, Programs}
import transcallang.Identifier

/**
  * @author tomer
  * @since 12/18/18
  */
class PatternRewriteRulePropSpec extends PropSpec with ScalaCheckPropertyChecks with Matchers {
  private implicit val hyperEdgeCreator: Arbitrary[HyperEdge[HyperTermId, HyperTermIdentifier]] = Arbitrary(hyperGraphEdgeGen)
  private implicit val hyperPatternCreator: Arbitrary[PatternRewriteRule.HyperPattern] = Arbitrary(hyperPatternGen)
  private implicit val rewriteRuleCreator: Arbitrary[PatternRewriteRule] = Arbitrary(rewriteRuleGen)
  private implicit val rewriteSearchStateCreator: Arbitrary[RewriteRule.HyperGraph] = Arbitrary(hyperGraphGen)

  property("Every state keep old edges") {
    // Not necessarily true because of compaction
    forAll { (rewriteRule: PatternRewriteRule, state: RewriteRule.HyperGraph) =>
      val oldState = state.clone
      rewriteRule.apply(state)
      (oldState.edges -- state.edges).isEmpty
    }
  }

//  property("If rewrites are equal then hash is equal") {
//    // Not necessarily true because of compaction
//    check(forAll { rewriteRule: RewriteRule => rewriteRule.hashCode == rewriteRule.copy().hashCode() })
//  }

  // TODO: We need to predict merges for this to work
  //  property("Every state adds edges") {
  //    check(forAll { (conditions: HyperPattern, destinations: HyperPattern) =>
  //      destinations.edges.forall(e1 => destinations.edges.forall(e2 => e1 == e2 || (e1.edgeType != e2.edgeType || e1.sources != e2.sources))) ==> {
  //        val dontcount = destinations.edges.filter(e1 => conditions.edges.exists(e2 => e1.edgeType == e2.edgeType && e1.sources == e2.sources))
  //        val filledConditions = HyperGraphManyWithOrderToOneLike.fillPattern[HyperTermId, HyperTermIdentifier, Int, HyperPattern](conditions, (Map.empty, Map.empty), () => HyperTermId(-1))
  //        val filledDestination = HyperGraphManyWithOrderToOneLike.fillPattern[HyperTermId, HyperTermIdentifier, Int, HyperPattern](destinations, (Map.empty, Map.empty), () => HyperTermId(-1))
  //        val tempToMerge = filledDestination.toSeq.map(e2 => (e2, filledDestination.filter(e1 => e1.edgeType == e2.edgeType && e1.sources == e2.sources))).filter(_._2.nonEmpty)
  //        val toMerge = {
  //          var alreadyMerged: Set[HyperTermId] = Set.empty
  //          var toAdd: Set[HyperTermId] = Set.empty
  //          for ((e, es) <- tempToMerge) {
  //            if (!alreadyMerged.contains(e.target)) {
  //              alreadyMerged = alreadyMerged ++ es.map(_.target)
  //              toAdd = toAdd + e.target
  //            }
  //          }
  //          tempToMerge.filter(toAdd contains _._1.target)
  //        }
  //        val rewriteRule = new RewriteRule(conditions, destinations, (a, b) => EmptyMetadata)
  //        val templateTermToHyperTermId: TemplateTerm[HyperTermId] => HyperTermId = RewriteRulePropSpec.mapper(Stream.from(0).map(HyperTermId).iterator)
  //        val templateTermToHyperTermIdentifier: TemplateTerm[HyperTermIdentifier] => HyperTermIdentifier = RewriteRulePropSpec.mapper(Stream.from(0).map(Identifier(_)).map(HyperTermIdentifier).iterator)
  //        val state = new RewriteSearchState(HyperGraphManyWithOrderToOne(filledConditions.toSeq: _*))
  //        val newState = rewriteRule.apply(state)
  //        val stateRemovingToMerge: RewriteSearchState.HyperGraph =
  //          toMerge.foldLeft(state.graph)((g, merges) =>
  //            merges._2.foldLeft(g)((g1, e) =>
  //              g1.mergeNodes(merges._1.target, e.target)
  //            )
  //          )
  //        (newState.graph.edges -- stateRemovingToMerge.edges).size == (destinations.edges -- conditions.edges).size - dontcount.size
  //      }
  //    })
  //  }

  property("Every state adds an edge") {
    forAll { (conditions: HyperPattern, destinationEdge: HyperEdge[HyperTermId, HyperTermIdentifier]) =>
      // Cant have illegal conditions or conditions containing destination
      whenever(conditions.edges.forall(e1 => (conditions.edges.toSeq :+ destinationEdge).forall(e2 => e1 == e2 || (e1.edgeType != e2.edgeType || e1.sources != e2.sources))) &&
        !generic.HyperGraph.fillPattern[HyperTermId, HyperTermIdentifier, Int](conditions, Match.empty, () => HyperTermId(-1)).contains(destinationEdge)) {
        val destination = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](ExplicitTerm(destinationEdge.target), ExplicitTerm(destinationEdge.edgeType), destinationEdge.sources.map(ExplicitTerm[HyperTermId]), EmptyMetadata)
        // At most one
        val willMerge = conditions.edges.find(e1 => e1.edgeType == destination.edgeType && e1.sources == destination.sources)
        val filledConditions = generic.HyperGraph.fillPattern[HyperTermId, HyperTermIdentifier, Int](conditions, Match.empty, () => HyperTermId(-1))

        val rewriteRule = new PatternRewriteRule(conditions, generic.HyperGraph(destination), _ => EmptyMetadata)
        val state = new RewriteRule.HyperGraph() ++= filledConditions
        val tempProgs = Programs(state.clone)
        rewriteRule.apply(state)

        if (willMerge.isEmpty) (state.edges -- tempProgs.queryGraph.edges).size == 1
        else (tempProgs.queryGraph.edges -- state.edges).forall(_.target == destinationEdge.target)
      }
    }
  }

  property("Can use repetition in conclusion") {
    forAll{(state: RewriteRule.HyperGraph) => whenever(state.nonEmpty) {
      val edge = HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](ReferenceTerm[HyperTermId](0), ReferenceTerm(1), Seq(Repetition.rep0(10, Stream.from(2).map(ReferenceTerm(_))).get), EmptyMetadata)
      val premise = CompactHyperGraph(edge)
      val conclusion = CompactHyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](edge.copy(edgeType = ExplicitTerm(HyperTermIdentifier(Identifier("00500")))))
      val rule = new PatternRewriteRule(premise.clone, conclusion.clone, _ => EmptyMetadata)
      val origSize = state.size
      rule(state)
      state.edges.size should be >= (origSize + state.edges.map(_.sources.size).toSet.size)
    }}
  }
}

object PatternRewriteRulePropSpec {
  def mapper[T <: HyperTerm](creator: Iterator[T]): TemplateTerm[T] => T = {
    val intToT = scala.collection.mutable.Map.empty[Int, T]

    def templateTermToT(templateTerm: TemplateTerm[T]): T = {
      templateTerm match {
        case ReferenceTerm(id) =>
          if (!intToT.contains(id)) {
            intToT(id) = creator.next()
          }
          intToT(id)
        case ExplicitTerm(hyperTerm) => hyperTerm
      }
    }

    templateTermToT
  }
}
