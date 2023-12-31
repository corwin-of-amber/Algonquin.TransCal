package synthesis.ui

import java.io.{PrintStream, File => JFile}

import org.rogach.scallop.ScallopOption
import synthesis._
import transcallang.{AnnotatedTree, Language, TranscalParser}

import scala.io.Source

/**
  * @author tomer
  * @since 11/24/18
  */
object Main extends App {
  import org.rogach.scallop.ScallopConf

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

  val conf = new CommandLineConfiguration(args.toIndexedSeq)
  val consolein = Source.createBufferedSource(System.in).getLines().filter(_ != "").map(_+ "\n").map(parser.apply)
  val optionalFile: ScallopOption[Iterator[AnnotatedTree]] = conf.file.map(readJFile)
  val userInput: Iterator[AnnotatedTree] = optionalFile.getOrElse(consolein)
  val userOutput: PrintStream = Console.out // conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
  val interpreter = new Interpreter(userInput, userOutput)

  val lastState = interpreter.start()

  val fullProgram = lastState.programs
  val hyperGraph = fullProgram.hyperGraph

  println(f"size: $hyperGraph.size")
  println(f"nodes: ${hyperGraph.nodes}")
  println(f"number of nodes: ${hyperGraph.nodes.size}")
  println("============================== In time complex ==============================")
  val timeComplexes = hyperGraph.filter(_.edgeType.identifier == Language.unionId).map(_.target).toSeq.flatMap(fullProgram.reconstructWithTimeComplex).map{case(tree, complexity) => (Programs.termToString(tree), complexity)}
  println(f"timecomplex edges ${hyperGraph.count(_.edgeType.identifier == Language.timeComplexId)} - total time complexities ${timeComplexes.size}")
  timeComplexes.foreach(println)
//  println("============================== Reconstruct ==============================")
//  hyperGraph.nodes.flatMap(
//    node => fullProgram.reconstruct(node).take(10).map(Programs.termToString).map((node, _))
//  ).foreach(println)
}
