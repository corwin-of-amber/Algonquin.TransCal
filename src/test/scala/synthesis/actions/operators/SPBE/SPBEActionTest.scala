package synthesis.actions.operators.SPBE

import org.scalatest.{FunSuite, Matchers}
import synthesis.Programs
import synthesis.rewrites.RewriteSearchState
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class SPBEActionTest extends FunSuite with Matchers {

  val listInt = AnnotatedTree.withoutAnnotations(Language.typeListId, Seq(Language.typeInt))
  val listInttoListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, listInt))
  val x = AnnotatedTree.identifierOnly(Identifier("x", Some(Language.typeInt)))
  val l = AnnotatedTree.identifierOnly(Identifier("l", Some(listInt)))
  val reverse = AnnotatedTree.identifierOnly(Identifier("reverse", annotation = Some(listInttoListInt)))
  val tru = AnnotatedTree.identifierOnly(Language.trueId)
  val fals = AnnotatedTree.identifierOnly(Language.falseId)

  test("testFindEquives") {
    fail()
  }

  test("testSygusStep can find reverse l") {
    val action = new SPBEAction(constantTeminals = Set(tru, fals, x), changingTerminals = Seq(Seq(l)), symbols = Set(reverse))
    val state = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val pattern = Programs.destructPattern(new TranscalParser().parseExpression("reverse _"))
    state.graph.findSubgraph[Int](pattern) should not be empty
  }

}
