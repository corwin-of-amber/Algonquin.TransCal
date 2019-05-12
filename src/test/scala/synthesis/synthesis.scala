import org.scalacheck.Gen
import org.scalacheck.Gen._
import transcallang.AnnotatedTree
import transcallang.Identifier

import scala.util.Random

package object synthesis {
  def TreeGenFactory(valuesSource: Gen[Identifier]): Gen[AnnotatedTree] = {
    val leafGen: Gen[AnnotatedTree] = valuesSource.map(AnnotatedTree.identifierOnly)

    def nodeGen(depth: Int): Gen[AnnotatedTree] = {
      val treeGen = if (depth == 0) leafGen else oneOf(leafGen, nodeGen(depth - 1))
      for {
        root <- valuesSource
        childrenSize = Random.nextInt(6) + 1
        children <- listOfN(childrenSize, treeGen)
      } yield AnnotatedTree(root, children, Seq.empty)
    }

    oneOf(leafGen, nodeGen(5))
  }


  val identifierGen: Gen[Identifier] = oneOf((0 to 50).map(x => Identifier(x.toString)))

  val identifierTreesGen: Gen[AnnotatedTree] = TreeGenFactory(identifierGen)

  val programsGen: Gen[Programs] = identifierTreesGen.map(Programs(_))
}
