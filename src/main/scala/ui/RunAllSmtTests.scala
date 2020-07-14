package ui

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import com.typesafe.scalalogging.LazyLogging
import lispparser.Translator
import synthesis.search.actions.thesy.Distributer
import ui.Main.conf

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.io.Source

object RunAllSmtTests extends App with LazyLogging {
  def readRunResults(file: File): RunResults = {
    val ois = new ObjectInputStream(new FileInputStream(new File(file.getAbsolutePath + ".res")))
    val res = ois.readObject().asInstanceOf[RunResults]
    ois.close()
    res
  }

  private object Locker


  // TODO: Add timeouts by surrounding the execution with a timer inside tasks of executioncontext
  val resourcePath = "src/main/resources/ind-problems/benchmarks-dt"
  val allFiles = new File(resourcePath).listFiles(_.isDirectory).flatMap(d => d.listFiles(f => f.isFile && f.getName.endsWith("smt2")))
  val knowResults: mutable.Map[String, RunResults] = mutable.Map(allFiles.collect({
    case f if new File(f.getAbsolutePath + ".res").exists() => (f.getName, readRunResults(f))
  }): _*)
  val files = allFiles.filterNot(f => knowResults.get(f.getName).exists(rr => rr.successGoals == rr.goals))
    .sortBy(_.getName.replace(".smt2", "").reverse.takeWhile(_.isDigit).reverse.toInt).par
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
  })
  val subTasks = tasks.flatMap(t => Distributer(t._3._1, 3).getExplorationTasks.flatten
    .flatMap({
      case (vocab, phCount) =>
        val newFileName = resourcePath + "/../" + "prerun_" + vocab.definitions.map(_.root.literal).sorted.mkString("_") + ".res"
        val newFile = new File(newFileName)
        if (newFile.exists()) {
          knowResults(newFileName) = readRunResults(newFile)
          None
        }
        else Some((newFile, phCount, (vocab, t._3._2, t._3._3)))
    }))
  val taskToGoal = (subTasks ++ tasks).map(t => (t._3._1, t._3._2, t._3._3, t._2, t._1)).groupBy(t => (t._1, t._2))
  taskToGoal.keys.toArray.sortBy(_._1.definitions.size).foreach({ case k@(vocab, ruleDefs) =>
    val f = taskToGoal(k).map(_._5).minBy(_.getAbsolutePath())
    val oosPath = f.getAbsolutePath
    var results = Set.empty[RunResults]
    Locker.synchronized({
      results = knowResults.values.toSet
    })
    val goals = taskToGoal(k).map(_._3)
    val phCount = taskToGoal(k).map(_._4).max
    val newRes = new ui.SmtlibInterperter().runExploration(vocab, goals.seq.flatten.toSet, ruleDefs, phCount, oosPath, results)
    Locker.synchronized({
      knowResults(f.getName) = newRes
    })
  })
}
