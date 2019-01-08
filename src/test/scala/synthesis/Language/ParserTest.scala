package synthesis.language

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Inspectors, Matchers}
import syntax.AstSugar.Term

import scala.io.Source

class ParserTest extends FunSuite with Matchers with Inspectors with LazyLogging {

  def treeToCode(t: Term) = {
    t.root
  }

  test("testApply") {
    val p = new OldParser
    val path = getClass.getResource("/").getPath + "../classes/examples"
    for (f <- new File(path).listFiles().filter(_.getName.endsWith(".tc"))) {
      logger.info(s"Working on $f")
      val text = Source.fromFile(f).getLines().mkString("\n")
      val term = p.apply(text)
      term shouldEqual p.apply(text)
    }
  }

}
