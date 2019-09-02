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
  test("sum example") {
    val testedAction = new RecursiveTimeComplexAction(Identifier("nodup'"), 2)
    val parser = new TranscalParser()
    val terms = List(
      parser.apply("nodup' ?w ?l = l match (⟨⟩ => true) / ((?x :: ?xs) => ({x} ∉ w) ∧ (nodup' ({x} ∪ w) xs)) [++]"),
      parser.apply("a1 -> ?nodup' {x} xs"),
      parser.apply("l = x :: xs [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} l 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} xs 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} w 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} l (len l) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} xs (len xs) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} w (len w) [++]"),
      parser.apply("a1 -> (_ ∉ {x}) ∧ ((_ ∪ _) ‖ _) ∧ nodup' _"),
    )
    val sumHyperGraph = new Interpreter(terms.iterator, System.out).start().programs
    val populated = RecursiveTimeComplexActionTest.populate(SpaceComplexRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules, sumHyperGraph.hyperGraph)
    val programs = Programs(populated)
    val size = programs.hyperGraph.size
    val rewriteRules = Set.empty[Operator[RewriteSearchState]]

    val newPrograms = testedAction.apply(ActionSearchState(programs, rewriteRules))
    val newSize = newPrograms.programs.hyperGraph.size

    newSize should be > size
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