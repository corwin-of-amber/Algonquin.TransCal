package synthesis

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import syntax.AstSugar.Term
import syntax.Identifier

class ProgramsPropSpec extends PropSpec with Checkers {

  implicit val termsCreator = Arbitrary(identifierTreesGen)
  implicit val programsCreator = Arbitrary(programsGen)

  property("main program is in reconstruct") {
    check(forAll { term: Term => {
      val programs = new Programs(term)
      programs.reconstruct(HyperTermIdentifier(term.root)).contains(term) :| programs.toString
    }
    })
  }

  property("every node is constructable") {
    check(forAll { programs: Programs =>
      !programs.hyperGraph.nodes.map(programs.reconstruct).exists(_.isEmpty)
    })
  }

  property("every edge is constructable") {
    check(forAll { programs: Programs =>
      !programs.hyperGraph.edgeTypes.map(programs.reconstruct).exists(_.isEmpty)
    })
  }

  property("unknown term returns empty iterator") {
    check(forAll { programs: Programs =>
      programs.reconstruct(HyperTermIdentifier(new Identifier("fail"))).isEmpty
    })
  }
}
