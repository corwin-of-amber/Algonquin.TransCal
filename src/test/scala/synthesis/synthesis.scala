import org.scalacheck.Gen
import org.scalacheck.Gen._
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}

import scala.util.Random

package object synthesis {
  def TreeGenFactory[Node](valuesSource: Gen[Node]): Gen[Tree[Node]] = {
    val leafGen: Gen[Tree[Node]] = valuesSource.map(new Tree(_))

    def nodeGen(depth: Int): Gen[Tree[Node]] = {
      val treeGen = if (depth == 0) leafGen else oneOf(leafGen, nodeGen(depth - 1))
      for {
        root <- valuesSource
        childrenSize = Random.nextInt(6) + 1
        children <- listOfN(childrenSize, treeGen)
      } yield new Tree[Node](root, children)
    }

    oneOf(leafGen, nodeGen(5))
  }


  val identifierGen: Gen[Identifier] = oneOf((0 to 50).map(new Identifier(_)))

  val identifierTreesGen: Gen[Term] = TreeGenFactory(identifierGen)

  val programsGen: Gen[Programs] = identifierTreesGen.map(Programs(_))
}
