package synthesis.ui

import java.io.{PrintStream, File => JFile}

import language.{OldParser, TranscalParser}
import org.rogach.scallop.ScallopOption
import syntax.AstSugar.Term

import scala.io.{BufferedSource, Source}

/**
  * @author tomer
  * @since 11/24/18
  */
object Main extends App {

  import org.rogach.scallop.ScallopConf

  class CommandLineConfiguration(arguments: Seq[String]) extends ScallopConf(arguments) {
    val file = opt[JFile]()
    validateFileIsFile(file)
    verify()
  }

  private def splitByStatements(term: Term): Iterator[Term] = {
    term.root match {
      case language.Language.semicolonId => term.subtrees.flatMap(splitByStatements).toIterator
      case _ => Iterator(term)
    }
  }

  val parser = new TranscalParser()
  val conf = new CommandLineConfiguration(args)
  val consolein = Source.createBufferedSource(System.in).getLines().map(parser.apply)
  val optionalFile: ScallopOption[Iterator[Term]] = conf.file.map(Source.fromFile).map(bs => splitByStatements(parser(bs.getLines().mkString("\n"))))
  val userInput: Iterator[Term] = optionalFile.getOrElse(consolein)
  val userOutput: PrintStream = conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
  val interpreter = new Interpreter(userInput, userOutput)
  interpreter.start
}
