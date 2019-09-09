package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import structures.{immutable, mutable}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.search.Operator
import synthesis.ui.Interpreter
import synthesis.{Programs, SpaceComplexRewriteRulesDB, TimeComplexRewriteRulesDB}
import transcallang.{Identifier, Language, TranscalParser}

class RecursiveTimeComplexActionTest extends FunSuite with Matchers  {
  test("nodup' example") {
    val testedAction = new RecursiveTimeComplexAction(Identifier("nodup'"), 2)
    val parser = new TranscalParser()
    val terms = List(
      parser.apply("nodup' ?w ?l = l match (⟨⟩ => true) / ((?x :: ?xs) => ({x} ∉ w) ∧ (nodup' ({x} ∪ w) xs)) [++]"),
      parser.apply("l = x :: xs [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} l 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} xs 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} w 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} l (len l) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} xs (len xs) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} w (len w) [++]"),
      parser.apply("timecomplexTrue-> a1 [only assoc]"),
      parser.apply("a1 -> timecomplex (nodup' _ _)"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()
    val populated = RecursiveTimeComplexActionTest.populate(SpaceComplexRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules, lastState.programs.hyperGraph)
    val programs = Programs(populated)
    val size = programs.hyperGraph.size

    val newPrograms = testedAction.apply(ActionSearchState(programs, Set.empty))
    val newSize = newPrograms.programs.hyperGraph.size
    newSize should be > size

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (nodup' ?w ?l) (len(l)) >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = newPrograms.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should have size(1)
  }

  test("sum example") {
    val testedAction = new RecursiveTimeComplexAction(Identifier("sum"), 2)
    val parser = new TranscalParser()
    val terms = List(
      parser.apply("sum ?l = l match (⟨⟩ => 0) / ((?x :: ?xs) => 1 + (sum xs)) [++]"),
      parser.apply("l = x :: xs [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} l 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} xs 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} l (len l) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} xs (len xs) [++]"),
      parser.apply("timecomplexTrue-> a1 [only assoc]"),
      parser.apply("a1 -> timecomplex (sum _ _)"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()
    val populated = RecursiveTimeComplexActionTest.populate(SpaceComplexRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules, lastState.programs.hyperGraph)
    val programs = Programs(populated)
    val size = programs.hyperGraph.size

    val newPrograms = testedAction.apply(ActionSearchState(programs, Set.empty))
    val newSize = newPrograms.programs.hyperGraph.size
    newSize should be > size

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (sum ?l) (len(l)) >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = newPrograms.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should have size(1)
  }
}

object RecursiveTimeComplexActionTest {
  def populate(rewriteRules: Set[Operator[RewriteSearchState]], hyperGraph: ActionSearchState.HyperGraph): ActionSearchState.HyperGraph = {
    var populated = RewriteSearchState(mutable.VersionedHyperGraph.empty ++ hyperGraph)
    var lastSize = 0
    var size = populated.graph.size
    do {
      for (rewriteRule <- rewriteRules) {
        populated = rewriteRule.apply(populated)
      }
      lastSize = size
      size = populated.graph.size
    } while (size != lastSize)
    immutable.VersionedHyperGraph.empty ++ populated.graph
  }
}