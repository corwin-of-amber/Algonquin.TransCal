import org.scalacheck.Gen
import org.scalacheck.Gen._
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}

import scala.util.Random

package object synthesis {
  def TreeGenFactory[Node](valuesSource: Gen[Node]): Gen[Tree[Node]] = for {
    root <- valuesSource
    childrenSize = Random.nextInt(6)
    children <- listOfN(childrenSize, valuesSource.map(new Tree(_)))
  } yield new Tree[Node](root, children)

  val identifierGen: Gen[Identifier] = oneOf((0 to 50).map(new Identifier(_)))

  val identifierTreesGen: Gen[Term] = TreeGenFactory(identifierGen)

  val programsGen: Gen[Programs] = identifierTreesGen.map(Programs(_))
}
