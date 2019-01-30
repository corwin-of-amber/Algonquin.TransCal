package synthesis.ui

import java.io.{PrintStream, File => JFile}

import language.{OldParser, TranscalParser}

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

  val conf = new CommandLineConfiguration(args)
  val consolein = Source.createBufferedSource(System.in)
  val userInput: BufferedSource = conf.file.map(Source.fromFile).getOrElse(consolein)
  val userOutput: PrintStream = conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
  val parser = new TranscalParser()
  val interpreter = new Interpreter(userInput, userOutput, parser)
  interpreter.start
}
