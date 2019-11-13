package synthesis.complexity

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import synthesis.complexity.Complexity._

/**
  * Property test for ComplexityOp.
  * @author Tomer
  * @since 20/05/19
  */
class ComplexityOpPropSpec extends PropSpec with ScalaCheckPropertyChecks with Matchers {
  val containerComplexityGen: Gen[ContainerComplexity] = Gen.alphaNumStr.map(ContainerComplexity(_))
  private implicit val containerComplexityCreator: Arbitrary[ContainerComplexity] = Arbitrary(containerComplexityGen)

  val logComplexityGen: Gen[LogComplexity] = containerComplexityGen.map(_.log.asInstanceOf)
  private implicit val logComplexityCreator: Arbitrary[LogComplexity] = Arbitrary(logComplexityGen)

  val polynomialComplexityGen: Gen[PolynomialComplexity] = containerComplexityGen.flatMap(a => containerComplexityGen.map(_ ^ a))
  private implicit val polynomialComplexityCreator: Arbitrary[PolynomialComplexity] = Arbitrary(polynomialComplexityGen)

  val addComplexityGen: Gen[AddComplexity] = containerComplexityGen.flatMap(a => containerComplexityGen.map(_ + a))
  private implicit val addComplexityCreator: Arbitrary[AddComplexity] = Arbitrary(addComplexityGen)

  val multipleComplexityGen: Gen[MultipleComplexity] = containerComplexityGen.flatMap(a => containerComplexityGen.map(_ * a))
  private implicit val multipleComplexityCreator: Arbitrary[MultipleComplexity] = Arbitrary(multipleComplexityGen)

  property("add of add is flatten") {
    forAll { addComplexity: AddComplexity =>
      (addComplexity + addComplexity).complexities.forall(!_.isInstanceOf[AddComplexity])
    }
  }

  property("mul of mul is flatten") {
    forAll { multipleComplexity: MultipleComplexity =>
      (multipleComplexity * multipleComplexity).complexities.forall(!_.isInstanceOf[MultipleComplexity])
    }
  }

  property("log of poly is flatten") {
    forAll { polynomialComplexity: PolynomialComplexity =>
      polynomialComplexity.log == polynomialComplexity.exponent
    }
  }
}
