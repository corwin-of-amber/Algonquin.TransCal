package ui

import java.io.{File, FileOutputStream, ObjectOutputStream, PrintStream}
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging
import lispparser.{LispParser, Translator}
//import lispparser.{sexpressionLexer, sexpressionParser}
import org.rogach.scallop.ScallopOption
import report.Stats
import transcallang.{AnnotatedTree, TranscalParser}

import scala.io.Source

/**
  * @author tomer
  * @since 11/24/18
  */
object Main extends App with LazyLogging {

  import org.rogach.scallop.ScallopConf

  println(s"Start time: ${Calendar.getInstance().getTime}")

  val parser = new TranscalParser()

  class CommandLineConfiguration(arguments: Seq[String]) extends ScallopConf(arguments) {
    val file: ScallopOption[File] = opt[File]()
    val smtin: ScallopOption[Boolean] = opt[Boolean]()

    validateFileIsFile(file)
    validateFileExists(file)
    verify()
  }

  private def splitByStatements(term: AnnotatedTree): Iterator[AnnotatedTree] =
    term.split(transcallang.Language.semicolonId).iterator

  def readLines(lines: Iterator[String]) = {
    val parser = new TranscalParser()
    splitByStatements(parser(lines.mkString("\n")))
  }

  def readFile(file: File): Iterator[AnnotatedTree] = {
    logger.warn(s"file name - ${file.getName}")
    val source = Source.fromFile(file)
    try {
      readLines(source.getLines())
    } finally {
      source.close()
    }
  }

  val conf = new CommandLineConfiguration(args.toIndexedSeq)

  if (conf.smtin.getOrElse(false)) {
    val source = Source.fromFile(conf.file())
    val text = source.getLines().mkString("\n")
    source.close()
    val res = new Translator(text).transcalScript
    new ui.SmtlibInterperter().apply(res, conf.file.apply().getAbsolutePath + ".res")
  } else {
    val consolein = Source.createBufferedSource(System.in).getLines().filter(_ != "").map(_ + "\n").map(parser.apply)
    val optionalFile: ScallopOption[Iterator[AnnotatedTree]] = conf.file.map(readFile)
    val userInput: Iterator[AnnotatedTree] =optionalFile.getOrElse(consolein)
    val userOutput: PrintStream = Console.out // conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
    val interpreter = new Interpreter(userInput, userOutput)
    val lastState = interpreter.start()

    val fullProgram = lastState.programs
    val hyperGraph = fullProgram.queryGraph

    println(f"size: $hyperGraph.size")
    println(f"nodes: ${hyperGraph.nodes}")
    println(f"number of nodes: ${hyperGraph.nodes.size}")
    println(s"End time: ${Calendar.getInstance().getTime}")
  }

  Stats.instance.dumpOnExit()
}
