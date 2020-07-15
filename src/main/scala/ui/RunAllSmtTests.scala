package ui

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import com.typesafe.scalalogging.LazyLogging
import lispparser.Translator
import report.Stats
import synthesis.search.actions.thesy.{Distributer, SortedVocabulary}
import transcallang.AnnotatedTree
import ui.Main.conf

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.io.Source

object RunAllSmtTests extends App with LazyLogging {
  def readRunResults(file: File): RunResults = {
    val fixedFile = if (file.getAbsolutePath.endsWith(".res")) file else new File(file.getAbsolutePath + ".res")
    val ois = new ObjectInputStream(new FileInputStream(fixedFile))
    val res = ois.readObject().asInstanceOf[RunResults]
    ois.close()
    res
  }

  private object Locker


  // TODO: Add timeouts by surrounding the execution with a timer inside tasks of executioncontext
  val resourcePath = "src/main/resources/ind-problems/benchmarks-dt"
  val files = new File(resourcePath).listFiles(_.isDirectory).flatMap(d => d.listFiles(f => f.isFile && f.getName.endsWith("smt2")))
  val resFiles = new File(resourcePath).listFiles(_.isDirectory).flatMap(d => d.listFiles(f => f.isFile && f.getName.endsWith("res")))
  val knowResults: mutable.Map[(SortedVocabulary, Set[AnnotatedTree]), RunResults] = mutable.Map(resFiles.collect({
    case f if f.exists() =>
      val results = readRunResults(f)
      ((new SortedVocabulary(results.knownTypes, results.knownRules), results.knownRulesDefs), results)
  }): _*)
  val tasks = files.flatMap(f => {
    val source = Source.fromFile(f)
    try {
      val res = (f, 2, new SmtlibInterperter().toVocabAndGoals(new Translator(source.getLines().mkString("\n")).transcalScript))
      source.close()
      Some(res)
    } catch {
      case e: IllegalArgumentException =>
        logger.warn(s"Skipping ${f.getAbsolutePath}, as it has a non equality assertion.")
        None
    }
  }).filterNot(t => knowResults.contains(t._3._1, t._3._2))

  val subTasks = tasks.flatMap(t => Distributer(t._3._1, 3).getExplorationTasks.flatten
    .flatMap({
      case (vocab, phCount) =>
        val newVocab = vocab.copy(new SmtlibInterperter().usedDatatypes(vocab.datatypes, t._3._2))
        val newFileName = t._1.getAbsolutePath.dropRight(4) + "_prerun_" + newVocab.definitions.head.root.literal + "_" +
          newVocab.datatypes.map(_.name.literal).toSeq.sorted.mkString("_") + ".res"
        val newFile = new File(newFileName)
        if (newFile.exists()) {
          knowResults((newVocab, t._3._2)) = readRunResults(newFile)
          None
        }
        else Some((newFile, phCount, (newVocab, t._3._2, t._3._3)))
    }))

//  val taskToGoal = (subTasks ++ tasks).seq.map(t => (t._3._1, t._3._2, t._3._3, t._2, t._1)).groupBy(t => (t._1, t._2))
  val taskToGoal = (tasks).seq.map(t => (t._3._1, t._3._2, t._3._3, t._2, t._1)).groupBy(t => (t._1, t._2))
  def runOne(vocab: SortedVocabulary, ruleDefs: Set[AnnotatedTree], reprove: Boolean) = {
    val k = (vocab, ruleDefs)
    val f = taskToGoal(k).map(_._5).minBy(_.getAbsolutePath())
    println(s"started ${f.getAbsolutePath}")
    val oosPath = if (f.getAbsolutePath.endsWith(".res")) f.getAbsolutePath else f.getAbsolutePath + ".res"
    var results = Set.empty[RunResults]
    Locker.synchronized({
      results = knowResults.values.toSet
    })
    val goals = taskToGoal(k).map(_._3)
    val phCount = taskToGoal(k).map(_._4).max
    val newRes = new ui.SmtlibInterperter().runExploration(vocab, goals.seq.flatten.toSet, ruleDefs, phCount, oosPath, results)
    Locker.synchronized({
      knowResults((vocab, ruleDefs)) = newRes
      val timingOut = new ObjectOutputStream(new FileOutputStream("smt-timings.bin"))
      timingOut.writeObject(Stats.Timing.dump)
      timingOut.close()
    })
  }

  taskToGoal.keys.filter(_._1.definitions.size == 1).par.map({ case k@(vocab, ruleDefs) =>
    runOne(vocab, ruleDefs, false)
  }).seq

  taskToGoal.keys.filterNot(_._1.definitions.size == 1).par.map({ case k@(vocab, ruleDefs) =>
    runOne(vocab, ruleDefs, true)
  }).seq
}
