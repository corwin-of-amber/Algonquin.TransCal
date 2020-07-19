import java.io.File

import synthesis.Programs
import ui.SmtlibInterperter

object resExtraction extends App {
  val smtin = new SmtlibInterperter
  for (d <- new File("src/main/resources/ind-problems/benchmarks-dt").listFiles(_.isDirectory)) {
    println("--------------------------------------------------------------")
    println(d.getName)
    val resFiles = d.listFiles(_.getName.endsWith(".res"))
    val results = smtin.readPreviousResults(resFiles.map(_.getCanonicalPath)).zip(resFiles).filter(t => t._2.getName.contains("goal") && t._1.goals.nonEmpty)
    println("file,goal,success")
    for((r, f) <- results) {
      assert(r.goals.size == 1)
      println(s"${f.getName},${r.goals.map({case (t1, t2) => Programs.termToString(t1) + " = " + Programs.termToString(t2)}).head},${r.successGoals == r.goals}")
    }
    println("--------------------------------------------------------------")
  }
}
