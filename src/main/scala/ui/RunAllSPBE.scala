package ui

import java.io.{PrintStream, File => JFile}
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging
import org.rogach.scallop.ScallopOption
import synthesis._
import transcallang.{AnnotatedTree, Language, TranscalParser}

import scala.io.Source

object RunAllSPBE extends App with LazyLogging {

  import org.rogach.scallop.ScallopConf

  println(s"Start time: ${Calendar.getInstance().getTime}")

  val parser = new TranscalParser()

  class CommandLineConfiguration(arguments: Seq[String]) extends ScallopConf(arguments) {
    val file: ScallopOption[JFile] = opt[JFile]()
    validateFileIsFile(file)
    validateFileExists(file)
    verify()
  }

  private def splitByStatements(term: AnnotatedTree): Iterator[AnnotatedTree] = {
    term.root match {
      case transcallang.Language.semicolonId => term.subtrees.flatMap(splitByStatements).iterator
      case _ => Iterator(term)
    }
  }

  def readLines(lines: Iterator[String]) = {
    val parser = new TranscalParser()
    splitByStatements(parser(lines.mkString("\n")))
  }

  def readJFile(jFile: JFile): Iterator[AnnotatedTree] = {
    val file = Source.fromFile(jFile)
    try {
      readLines(file.getLines())
    } finally {
      file.close()
    }
  }

  def getBenchmarks: Array[JFile] = {
    for (path <- List(getClass.getResource("/timings").getPath, "./src/main/resources/timings")) {
      val files = new JFile(path).listFiles()
      if (files != null) return files
    }
    throw new Error("cannot find `timings` directory")
  }

  val allFiles = getBenchmarks.filter(f => f.isFile && f.getName.startsWith("RunSpbe"))
  for (f <- allFiles) {
    //  val conf = new CommandLineConfiguration(args.toIndexedSeq)
    //  val consolein = Source.createBufferedSource(System.in).getLines().filter(_ != "").map(_+ "\n").map(parser.apply)
    val file: Iterator[AnnotatedTree] = readJFile(f)
    logger.warn(s"file name - ${f.getName}")
    //  val userInput: Iterator[AnnotatedTree] = optionalFile.getOrElse(consolein)
    val userOutput: PrintStream = Console.out // conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
    val interpreter = new Interpreter(file, userOutput)

    val lastState = interpreter.start()

    println("============================== In time complex ==============================")
    println(s"End time: ${Calendar.getInstance().getTime}")
    logger.warn(s"finished file")
  }
}
