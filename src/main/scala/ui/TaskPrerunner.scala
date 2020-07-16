package ui

import java.io.{File, FileInputStream, ObjectInputStream}
import java.util.Locale.LanguageRange

import lispparser.Translator
import synthesis.search.actions.thesy.{Distributer, SortedVocabulary}
import transcallang.{AnnotatedTree, Language}

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
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
  val allVocabsAndDefs = files.flatMap(f => {
    val source = Source.fromFile(f)
    val res = try {
      Some(new SmtlibInterperter().toVocabAndGoals(new Translator(source.getLines().mkString("\n")).transcalScript))
    } catch {
      case _ => None
    }
    source.close()
    res
  })


  val vocabAndDef = {
    val res = mutable.Map.empty[AnnotatedTree, Set[(SortedVocabulary, Set[AnnotatedTree])]].withDefault(k => Set.empty)
    allVocabsAndDefs.foreach({case (vocab, defs, goals) =>
      val inter = new SmtlibInterperter()
      val defed = inter.defdFunctions(vocab.definitions, defs)
      defed.foreach(df => {
        val relevantDefs = defs.filter(f => f.nodes.exists(_.root.literal == df.root.literal))
        val filteredVocab = SortedVocabulary(inter.usedDatatypes(vocab.datatypes, relevantDefs), Set(df))
        res(df) = res(df) ++ Set((filteredVocab, relevantDefs))
      })
    })
    res.mapValues(s => s.minBy(_._2.size))
  }

  val singleRunRes = vocabAndDef.toSeq.flatMap({ case (k: AnnotatedTree, (vocab: SortedVocabulary, defs: Set[AnnotatedTree])) =>
    val oospath = s"$resourcePath/${vocab.definitions.head.root.literal}.res"
    if (new File(oospath).exists()) Some(k -> readRunResults(new File(oospath)))
    else None
  }).toMap

//  val singleRunRes = vocabAndDef.toSeq.par.map({ case (k: AnnotatedTree, (vocab: SortedVocabulary, defs: Set[AnnotatedTree])) =>
//    val inter = new SmtlibInterperter()
//    val oospath = s"$resourcePath/${vocab.definitions.head.root.literal}.res"
//    if (new File(oospath).exists()) k -> readRunResults(new File(oospath))
//    else {
//      println(s"starting $oospath")
//      val phCount = if(vocab.allSymbols.exists(_.getType.exists(t => t.root == Language.mapTypeId && t.subtrees.length >= 4))) 2 else 3
//      k -> inter.runExploration(vocab, Set.empty, defs, phCount, s"$resourcePath/${vocab.definitions.head.root.literal}.res", Set.empty, false)
//    }
//  }).seq.toMap

  val coupleRunRes = vocabAndDef.keys.toSeq.combinations(2).toSet.par
    .filter(fs => allVocabsAndDefs.exists({case (vocab, defs, goals) => fs.diff(vocab.definitions.toSeq).isEmpty}))
    .map(fs => {
      val vocabulary = SortedVocabulary(fs.flatMap(f => vocabAndDef(f)._1.datatypes).toSet,
        fs.flatMap(f => vocabAndDef(f)._1.definitions).toSet)
      val defs = fs.flatMap(f => vocabAndDef(f)._2).toSet
      val inter = new SmtlibInterperter()
      val oospath = s"$resourcePath/${vocabulary.definitions.map(_.root.literal).mkString("_")}.res"
      if (new File(oospath).exists()) readRunResults(new File(oospath))
      else {
        println(s"starting $oospath")
        inter.runExploration(vocabulary, Set.empty, defs, 2, oospath, fs.flatMap(singleRunRes.get).toSet, true)
      }
//      inter.runExploration(vocabulary, Set.empty, defs, 2, s"$resourcePath/${vocabulary.definitions.map(_.root.literal).mkString("_")}.res", singleRunRes.values.toSet)
    }).seq

}
