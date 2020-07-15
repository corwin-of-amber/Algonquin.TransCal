package ui

import java.io.{File, FileInputStream, ObjectInputStream}

import lispparser.Translator
import synthesis.search.actions.thesy.{Distributer, SortedVocabulary}
import transcallang.AnnotatedTree

import scala.collection.mutable
import scala.io.Source

object TaskPrerunner extends App {
  def readRunResults(file: File): RunResults = {
    val fixedFile = if (file.getAbsolutePath.endsWith(".res")) file else new File(file.getAbsolutePath + ".res")
    val ois = new ObjectInputStream(new FileInputStream(fixedFile))
    val res = ois.readObject().asInstanceOf[RunResults]
    ois.close()
    res
  }

  def filterDefs(funDecl: AnnotatedTree, funDefs: Set[AnnotatedTree]) = {
    funDefs.filter(_.nodes.exists(_.root.literal == funDecl.root.literal))
  }

  // TODO: Add timeouts by surrounding the execution with a timer inside tasks of executioncontext
  val resourcePath = "src/main/resources/ind-problems/benchmarks-dt/clam"
  val files = new File(resourcePath).listFiles(f => f.isFile && f.getName.endsWith("smt2"))
  val resFiles = new File(resourcePath).listFiles(f => f.isFile && f.getName.endsWith("res"))
  val vocabsAndDefs = files.flatMap(f => {
    val source = Source.fromFile(f)
    val res = try {
      Some(new SmtlibInterperter().toVocabAndGoals(new Translator(source.getLines().mkString("\n")).transcalScript))
    } catch {
      case _ => None
    }
    source.close()
    res
  })


  val funcToVocabAndDefs = {
    val res = mutable.Map.empty[AnnotatedTree, Set[(SortedVocabulary, Set[AnnotatedTree])]].withDefault(k => Set.empty)
    vocabsAndDefs.foreach({case (vocab, defs, goals) =>
      val inter = new SmtlibInterperter()
      val defed = inter.defdFunctions(vocab.definitions, defs)
      defed.foreach(df => {
        val relevantDefs = defs.filter(f => f.nodes.exists(_.root.literal == df.root.literal))
        val filteredVocab = SortedVocabulary(inter.usedDatatypes(vocab.datatypes, relevantDefs), Set(df))
        res(df) = res(df) ++ Set((filteredVocab, relevantDefs))
      })
    })
    val updated = res.values.map(s => s.minBy(_._2.size)).toSet
    updated.par.map({ case (vocab, defs) =>
      val inter = new SmtlibInterperter()
      inter.runExploration(vocab, Set.empty, defs, 3, s"$resourcePath/${vocab.definitions.head.root.literal}.res", Set.empty)
    })
  }
}
