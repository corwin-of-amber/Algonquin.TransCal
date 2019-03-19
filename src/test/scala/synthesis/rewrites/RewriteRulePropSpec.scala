package synthesis.rewrites

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures._
import structures.immutable.{HyperGraphManyWithOrderToOne, VersionedHyperGraph}
import syntax.Identifier
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 12/18/18
  */
class RewriteRulePropSpec extends PropSpec with Checkers {
  private implicit val hyperEdgeCreator = Arbitrary(hyperGraphEdgeGen)
  private implicit val hyperPatternCreator = Arbitrary(hyperPatternGen)
  private implicit val rewriteRuleCreator = Arbitrary(rewriteRuleGen)
  private implicit val rewriteSearchStateCreator = Arbitrary(rewriteSearchStateGen)

  property("Every state keep old edges") {
    // Not necessarily true because of compaction
    check(forAll { (rewriteRule: RewriteRule, state: RewriteSearchState) => {
      val newState = rewriteRule.apply(state)
      (state.graph.edges -- newState.graph.edges).isEmpty
    }
    })
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
  //        val templateTermToHyperTermIdentifier: TemplateTerm[HyperTermIdentifier] => HyperTermIdentifier = RewriteRulePropSpec.mapper(Stream.from(0).map(new Identifier(_)).map(HyperTermIdentifier).iterator)
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
    check(forAll { (conditions: HyperPattern, destinationEdge: HyperEdge[HyperTermId, HyperTermIdentifier]) =>
      // Cant have illegal conditions or conditions containing destination
      (conditions.edges.forall(e1 => (conditions.edges.toSeq :+ destinationEdge).forall(e2 => e1 == e2 || (e1.edgeType != e2.edgeType || e1.sources != e2.sources))) &&
        !HyperGraphManyWithOrderToOneLike.fillPattern[HyperTermId, HyperTermIdentifier, Int, HyperPattern](conditions, (Map.empty, Map.empty), () => HyperTermId(-1)).contains(destinationEdge)) ==> {
        val destination = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](ExplicitTerm(destinationEdge.target), ExplicitTerm(destinationEdge.edgeType), destinationEdge.sources.map(ExplicitTerm[HyperTermId]), EmptyMetadata)
        // At most one
        val willMerge = conditions.edges.find(e1 => e1.edgeType == destination.edgeType && e1.sources == destination.sources)
        val filledConditions = HyperGraphManyWithOrderToOneLike.fillPattern[HyperTermId, HyperTermIdentifier, Int, HyperPattern](conditions, (Map.empty, Map.empty), () => HyperTermId(-1))
        val filledDestination = HyperGraphManyWithOrderToOne(destinationEdge)

        val rewriteRule = new RewriteRule(conditions, HyperGraphManyWithOrderToOne(destination), (a, b) => EmptyMetadata)
        val templateTermToHyperTermId: TemplateTerm[HyperTermId] => HyperTermId = RewriteRulePropSpec.mapper(Stream.from(0).map(HyperTermId).iterator)
        val templateTermToHyperTermIdentifier: TemplateTerm[HyperTermIdentifier] => HyperTermIdentifier = RewriteRulePropSpec.mapper(Stream.from(0).map(new Identifier(_)).map(HyperTermIdentifier).iterator)
        val state = new RewriteSearchState(VersionedHyperGraph(filledConditions.toSeq: _*))
        val newState = rewriteRule.apply(state)

        if (willMerge.isEmpty) (newState.graph.edges -- state.graph.edges).size == 1
        else (state.graph.edges -- newState.graph.edges).forall(_.target == destinationEdge.target)
      }
    })
  }
}

object RewriteRulePropSpec {
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
