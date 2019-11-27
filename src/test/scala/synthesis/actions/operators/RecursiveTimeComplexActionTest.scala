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

  test("nodup' example textual") {
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
      parser.apply(f"${Language.recursiveTimeComplexId.literal} nodup' 2"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (nodup' ?w ?l) (len(l)) >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = lastState.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should have size (1)
  }

  test("sum example") {
    val testedAction = new RecursiveTimeComplexAction(Identifier("sum"), 1)
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

  test("sum example textual") {
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
      parser.apply(f"${Language.recursiveTimeComplexId.literal} sum 1"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (sum ?l) (len(l)) >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = lastState.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should have size(1)
  }

  test("map example textual") {
    val parser = new TranscalParser()
    val terms = List(
      parser.apply("map ?f ?l = l match (⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (f x) :: (map f xs)) [++]"),
      parser.apply("l = x :: xs [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} l 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} xs 0 [++]"),
      parser.apply(f"(?g (x) ||| ?z) |>> ${Language.timeComplexTrueId.literal} ||| ${Language.timeComplexId.literal} ( z ) 1"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} l (len l) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} xs (len xs) [++]"),
      parser.apply(f"(?g (x) ||| ?z) |>> ${Language.spaceComplexTrueId.literal} ||| ${Language.spaceComplexId.literal} ( z ) 1"),
      parser.apply(f"${Language.recursiveTimeComplexId.literal} map 2"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (map ?f ?l) _ >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = lastState.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should not be empty
  }

  test("filter example textual") {
    val parser = new TranscalParser()
    val terms = List(
      parser.apply("filter ?p ?l = l match (⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match (true => (filter p xs)) / (false => x :: (filter p xs))) [++]"),
      parser.apply("l = x :: xs [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} l 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} xs 0 [++]"),
      parser.apply(f"(?g (x) ||| ?z) |>> ${Language.timeComplexTrueId.literal} ||| ${Language.timeComplexId.literal} ( z ) 1"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} l (len l) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} xs (len xs) [++]"),
      parser.apply(f"(?g (x) ||| ?z) |>> ${Language.spaceComplexTrueId.literal} ||| ${Language.spaceComplexId.literal} ( z ) 1"),
      parser.apply(f"${Language.recursiveTimeComplexId.literal} filter 2"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (filter ?p ?l) _ >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = lastState.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should not be empty
  }

  test("mapFilter example textual") {
    val parser = new TranscalParser()
    val terms = List(
      parser.apply("mapFilter ?f ?p ?l = map f (filter p l) [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} l 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.timeComplexTrueId.literal} = ${Language.timeComplexId.literal} xs 0 [++]"),
      parser.apply(f"((map ?f ?l) ||| ?z) |>> ${Language.timeComplexTrueId.literal} ||| ${Language.timeComplexId.literal} ( z ) (len l)"),
      parser.apply(f"((filter ?p ?l) ||| ?z) |>> ${Language.timeComplexTrueId.literal} ||| ${Language.timeComplexId.literal} ( z ) (len l)"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} l (len l) [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} x 0 [++]"),
      parser.apply(f"${Language.spaceComplexTrueId.literal} = ${Language.spaceComplexId.literal} xs (len xs) [++]"),
      parser.apply(f"((map ?f ?l) ||| ?z) |>> ${Language.spaceComplexTrueId.literal} ||| ${Language.spaceComplexId.literal} ( z ) (len l)"),
      parser.apply(f"((filter ?p ?l) ||| ?z) |>> ${Language.spaceComplexTrueId.literal} ||| ${Language.spaceComplexId.literal} ( z ) (len l)"),
      parser.apply(f"${Language.recursiveTimeComplexId.literal} mapFilter 3"),
    )
    val lastState = new Interpreter(terms.iterator, System.out).start()

    val wantedGraph = new LetAction(parser.apply(f"${Language.timeComplexId.literal} (mapFilter ?f ?p ?l) _ >> ${Language.timeComplexTrueId.literal}")).rules.head.premise
    val found = lastState.programs.hyperGraph.findSubgraph[Int](wantedGraph)
    found should not be empty
  }
}

object RecursiveTimeComplexActionTest {
  def populate(rewriteRules: Set[Operator[RewriteSearchState]], hyperGraph: ActionSearchState.HyperGraph): ActionSearchState.HyperGraph = {
    var populated = RewriteSearchState(mutable.CompactHyperGraph.empty ++ hyperGraph)
    var lastSize = 0
    var size = populated.graph.size
    do {
      for (rewriteRule <- rewriteRules) {
        populated = rewriteRule.apply(populated)
      }
      lastSize = size
      size = populated.graph.size
    } while (size != lastSize)
    immutable.CompactHyperGraph.empty ++ populated.graph
  }
}