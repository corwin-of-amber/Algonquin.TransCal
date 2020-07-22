package ui

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, PrintStream}
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging
import lispparser.{LispParser, Translator}
import synthesis.Programs
import synthesis.search.actions.thesy.{Prover, TheoryExplorationAction}
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
    val dir: ScallopOption[File] = opt[File]()
    val smtin: ScallopOption[Boolean] = opt[Boolean]()
    val justCheck: ScallopOption[Boolean] = opt[Boolean]()
    val previousResults: ScallopOption[List[String]] = opt[List[String]](default = Some(List()))
    val libraryLocations: ScallopOption[List[String]] = opt[List[String]](default = Some(List()))

    validateFileIsFile(file)
    validateFileIsDirectory(dir)
    validateFileExists(file)
    verify()
  }

  val conf = new CommandLineConfiguration(args.toIndexedSeq)
  transcallang.System.path.prepend(conf.libraryLocations(): _*)

  private def splitByStatements(term: AnnotatedTree): Iterator[AnnotatedTree] = {
    term.split(transcallang.Language.semicolonId).iterator
  }

  def readFile(name: String): Iterator[AnnotatedTree] = {
    val updatedName = if(name.endsWith(".tc")) name else name + ".tc"

    transcallang.System.path
      .find(lib => new File(new File(lib), updatedName).exists())
      .map(s => readFile(new File(new File(s), updatedName))).get
  }

  def readFile(file: File): Iterator[AnnotatedTree] = {
    logger.warn(s"file name - ${file.getName}")
    val source = Source.fromFile(file)
    try {
      val parser = new TranscalParser()
      splitByStatements(parser(source.getLines().mkString("\n")))
    } finally {
      source.close()
    }
  }

  if (conf.smtin.getOrElse(false)) {

    val smtin = new ui.SmtlibInterperter()
//    val prevResults = conf.previousResults.map(l => smtin.readPreviousResults(l)).getOrElse(Set.empty[RunResults])
    val prevResults = conf.dir.map(_.listFiles(_.getName.endsWith(".res"))).getOrElse(Array(new File(conf.file().getCanonicalPath + ".res")).filter(_.exists()))
    if (!conf.justCheck()) {
      val goals = conf.dir.map(_.listFiles(_.getName.endsWith(".smt2"))).getOrElse(Array(conf.file()))
      goals.filterNot(f => {
        prevResults.exists(p => {
          val res = p.getAbsolutePath.contains(f.getAbsolutePath) && {
            val pr = smtin.readPreviousResults(Seq(p.getAbsolutePath))
            pr.head.goals == pr.head.successGoals
          }
          if(res) println(s"Done with ${f.getName}")
          res
        })
      }).par.map(g =>{
        val source = Source.fromFile(g)
        val text = source.getLines().mkString("\n")
        source.close()
        println(s"working on ${g.getName}")
        val res = new Translator(text).transcalScript
        smtin(res, g.getAbsolutePath + ".res", smtin.readPreviousResults(prevResults.map(f => f.getAbsolutePath)))
      })
    }
    else {
      val source = Source.fromFile(conf.file())
      val text = source.getLines().mkString("\n")
      source.close()
      val res = new Translator(text).transcalScript
      smtin.check(res, Seq(conf.file().getAbsolutePath + ".res") ++ conf.previousResults.getOrElse(List.empty[String]))
    }
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
