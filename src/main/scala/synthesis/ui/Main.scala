package synthesis.ui

import java.io.{BufferedReader, PrintStream, File => JFile}

import synthesis.language.OldParser

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
    verify()
  }

  val conf = new CommandLineConfiguration(args)
  val userInput: BufferedReader = conf.file.map(Source.fromFile).map(_.bufferedReader()).getOrElse(Console.in)
  val userOutput: PrintStream = conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
  val parser = new OldParser()
  val interpreter = new Interpreter(userInput, userOutput, parser)
  interpreter.start
}
