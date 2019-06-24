package synthesis.ui

import java.io.{PrintStream, File => JFile}

import org.rogach.scallop.ScallopOption
import synthesis.Programs
import synthesis.complexity.Complexity._
import synthesis.complexity.{Complexity, ComplexityPartialOrdering, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteRule
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

import scala.io.Source
import scala.util.Try

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
      case transcallang.Language.semicolonId => term.subtrees.flatMap(splitByStatements).toIterator
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

  val conf = new CommandLineConfiguration(args)
  val consolein = Source.createBufferedSource(System.in).getLines().filter(_ != "").map(_+ "\n").map(parser.apply)
  val optionalFile: ScallopOption[Iterator[AnnotatedTree]] = conf.file.map(readJFile)
  val userInput: Iterator[AnnotatedTree] = optionalFile.getOrElse(consolein)
  val userOutput: PrintStream = Console.out // conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
  val interpreter = new Interpreter(userInput, userOutput)

  val lastState = interpreter.start()

  implicit object AnnotatedTreeByDepth extends Ordering[AnnotatedTree] {
    override def compare(x: AnnotatedTree, y: AnnotatedTree): Int = x.toString.compareTo(y.toString)
  }
  val TIMECOMPLEX = "timecomplex"
  val TIMECOMPLEX_IDENTIFIER = Identifier(TIMECOMPLEX)
  val TIMECOMPLEX_PATTERN: RewriteRule.HyperPattern = Programs.destructPattern(new TranscalParser().parseExpression(f"$TIMECOMPLEX(_, _)"))

  def calculateComplex(tree: AnnotatedTree): Complexity = {
    tree.root.literal match {
      case "+" =>
        val first = calculateComplex(tree.subtrees.head)
        val second = calculateComplex(tree.subtrees(1))
        val added = first + second
        added
      case literal: String if Try(literal.toInt).isSuccess => ConstantComplexity(literal.toInt)
      case _ => ContainerComplexity(Programs.termToString(tree))
    }
  }

  object FullComplexityPartialOrdering extends Ordering[Complexity] {
    override def compare(x: Complexity, y: Complexity): Int = {
      val res = ComplexityPartialOrdering.tryCompare(x, y)
        res.getOrElse(y.toString compare x.toString)
    }
  }

  val fullProgram = lastState.programs
  val hyperGraph = fullProgram.hyperGraph
  val timeComplexEdges = hyperGraph.edges.filter(_.edgeType.identifier == TIMECOMPLEX_IDENTIFIER)
  val typeEdges = hyperGraph.edges.filter(_.edgeType.identifier == Language.typeId)
  val size = hyperGraph.size
  println(f"size: $size")
  val nonComplexNodes = {
    val matchNodes = hyperGraph.edges.filter(_.edgeType.identifier == Identifier("match")).map(_.target)
    val lambdaNodes = hyperGraph.edges.filter(_.edgeType.identifier == Identifier("⇒")).map(_.target)
    val typeNodes = hyperGraph.edges.filter(_.edgeType.identifier == Identifier("type")).map(_.target)
    hyperGraph.nodes -- timeComplexEdges.flatMap(e => Seq(e.target, e.sources(1))) -- matchNodes -- typeNodes -- lambdaNodes
  }
  println(f"nodes: $nonComplexNodes")
  println(f"number of nodes: ${nonComplexNodes.size}")
  timeComplexEdges.toStream.flatMap(e => fullProgram.reconstructWithPattern(e.target, TIMECOMPLEX_PATTERN))
    .map(tree=> (Programs.termToString(tree.subtrees.head), calculateComplex(tree.subtrees(1))))
    .groupBy(_._1).map(a => a.copy(_2 = a._2.map(_._2).min(FullComplexityPartialOrdering)))
    .toList.sortBy(_._1.length)
    .foreach(println)

  val timeComplexStrings = timeComplexEdges.flatMap(e => fullProgram.reconstructWithPattern(e.target, TIMECOMPLEX_PATTERN))
    .map(tree => Programs.termToString(tree.subtrees.head))
  println("============================== Not in complex ==============================")
  nonComplexNodes.flatMap(fullProgram.reconstruct).map(Programs.termToString)
    .filterNot(timeComplexStrings.contains)
    .toList.sortBy(_.length)
    .foreach(println)
}
