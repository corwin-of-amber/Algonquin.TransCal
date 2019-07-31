package synthesis.ui

import java.io.{PrintStream, File => JFile}

import org.rogach.scallop.ScallopOption
import structures.Explicit
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.complexity.Complexity._
import synthesis.complexity.{Complexity, ComplexityPartialOrdering, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteRule.RewriteRuleMetadata
import transcallang.{AnnotatedTree, Identifier, TranscalParser}

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

  implicit object AnnotatedTreeByDepth extends Ordering[AnnotatedTree] {
    override def compare(x: AnnotatedTree, y: AnnotatedTree): Int = x.toString.compareTo(y.toString)
  }
  val TIMECOMPLEX = "timecomplex"
  val TIMECOMPLEX_IDENTIFIER = Identifier(TIMECOMPLEX)

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
  println(f"size: $hyperGraph.size")
  println(f"nodes: ${hyperGraph.nodes}")
  println(f"number of nodes: ${hyperGraph.nodes.size}")
  println("============================== In time complex ==============================")
  val timeComplexes = hyperGraph.nodes.toSeq.flatMap(fullProgram.reconstructWithTimeComplex).map{case(tree, complexity) => (Programs.termToString(tree), complexity)}
  println(f"timecomplex edges ${hyperGraph.count(_.edgeType.identifier == TIMECOMPLEX_IDENTIFIER)} - total time complexities ${timeComplexes.size}")
  timeComplexes.foreach(println)
  println("============================== In space complex ==============================")
  val spaceComplexes = hyperGraph.toSeq.filter(_.edgeType.identifier == Identifier("spacecomplex")).map(_.sources.head).flatMap(fullProgram.reconstruct).map(Programs.termToString)
  println(f"spacecomplex edges ${hyperGraph.count(_.edgeType.identifier == Identifier("spacecomplex"))} - total space complexities ${spaceComplexes.size}")
  spaceComplexes.foreach(println)
  println("============================== In complex - alt ==============================")
  for(edge <- hyperGraph) {
    if (edge.edgeType.identifier == TIMECOMPLEX_IDENTIFIER) {
      for ( complexTree <- fullProgram.reconstruct(edge.sources(1)); tree <- fullProgram.reconstruct(edge.sources.head)) {
        println((edge.sources.head.id, Programs.termToString(tree), Programs.termToString(complexTree)))
      }
      edge.metadata.toSeq.collectFirst {
        case x: RewriteRuleMetadata =>
          for (originalEdge <- x.originalEdges) {
            println("  " + (
              originalEdge.target.asInstanceOf[Explicit[HyperTermId, Int]].value.id,
              originalEdge.edgeType.asInstanceOf[Explicit[HyperTermIdentifier, Int]].value.identifier,
              originalEdge.sources.map(_.asInstanceOf[Explicit[HyperTermId, Int]].value.id)
            ))
          }
      }
    }
  }
}
