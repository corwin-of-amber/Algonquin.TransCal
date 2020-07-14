package ui

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import lispparser.Translator
import ui.Main.conf

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.io.Source

object RunAllSmtTests extends App {
  def readRunResults(file: File): RunResults = {
    val ois = new ObjectInputStream(new FileInputStream(new File(file.getAbsolutePath + ".res")))
    val res = ois.readObject().asInstanceOf[RunResults]
    ois.close()
    res
  }

  private object Locker

  // TODO: Add timeouts by surrounding the execution with a timer inside tasks of executioncontext
  val resourcePath = "src/main/resources/ind-problems/benchmarks-dt"
  new File(resourcePath).listFiles(_.isDirectory).par.map(d => {
    val files = d.listFiles(f => f.isFile && f.getName.endsWith("smt2"))
    val knowResults: mutable.Map[String, RunResults] = mutable.Map(files.collect({
      case f if new File(f.getAbsolutePath + ".res").exists() => (f.getName, readRunResults(f))
    }): _*)
    // TODO: add ordering to tasks
    val tasks = files.filterNot(f => knowResults.get(f.getName).exists(rr => rr.success)).par
    tasks.foreach(f => {
      val source = Source.fromFile(f)
      val text = source.getLines().mkString("\n")
      source.close()
      val res = new Translator(text).transcalScript
      val oosPath = f.getAbsolutePath + ".res"
      var results = Set.empty[RunResults]
      Locker.synchronized({
        results = knowResults.values.toSet
      })
      val newRes = new ui.SmtlibInterperter().apply(res, oosPath, results)
      Locker.synchronized({
        knowResults(f.getName) = newRes
      })
    })
  })

}
