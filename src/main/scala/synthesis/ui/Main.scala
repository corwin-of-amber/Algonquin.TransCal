package synthesis.ui

import java.io.{PrintStream, File => JFile}

import transcallang.{Identifier, TranscalParser}
import org.rogach.scallop.ScallopOption
import syntax.Tree

import scala.io.Source

/**
  * @author tomer
  * @since 11/24/18
  */
object Main extends App {

  import org.rogach.scallop.ScallopConf

  class CommandLineConfiguration(arguments: Seq[String]) extends ScallopConf(arguments) {
    val file = opt[JFile]()
    validateFileIsFile(file)
    validateFileExists(file)
    verify()
  }

  private def splitByStatements(term: Tree[Identifier]): Iterator[Tree[Identifier]] = {
    term.root match {
      case transcallang.Language.semicolonId => term.subtrees.flatMap(splitByStatements).toIterator
      case _ => Iterator(term)
    }
  }

  val parser = new TranscalParser()
  val conf = new CommandLineConfiguration(args)
  val consolein = Source.createBufferedSource(System.in).getLines().filter(_ != "").map(_+ "\n").map(parser.apply)
  val optionalFile: ScallopOption[Iterator[Tree[Identifier]]] = conf.file.map(Source.fromFile).map(bs => splitByStatements(parser(bs.getLines().mkString("\n"))))
  val userInput: Iterator[Tree[Identifier]] = optionalFile.getOrElse(consolein)
  val userOutput: PrintStream = Console.out // conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
  val interpreter = new Interpreter(userInput, userOutput)
  interpreter.start
}
