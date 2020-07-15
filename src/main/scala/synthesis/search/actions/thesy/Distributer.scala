package synthesis.search.actions.thesy

import synthesis.search.ActionSearchState
import synthesis.search.rewrites.RewriteRule
import transcallang.AnnotatedTree

import scala.collection.mutable

/** Create a set of sub-exploration tasks to run with more placeholders.
  * These tasks will usually be common to many goals as function definitions repeat and can therefore be cached.
  *
  * @param vocabulary Sorted vocabulary of the original exploration task
  */
case class Distributer(vocabulary: SortedVocabulary, exampleDepth: Int) {
  private val rules = mutable.Set.empty[AnnotatedTree]
  def foundRules: Set[AnnotatedTree] = rules.toSet

  /** Split into multiple exploration tasks.
    * Each is a smaller sorted vocabulary with the accompanying necassery placeholder count
    *
    * @return Exploration tasks splitted into sequantial dependant sets of parallel tasks.
    */
  def getExplorationTasks: Seq[Set[(SortedVocabulary, Int)]] = {
    val dtToFToCount = vocabulary.datatypes.map(dt => {
      (dt,
        vocabulary.definitions.map(d => (d, d.getType.get.subtrees.dropRight(1).count(dt.asType == _))).toMap)
    }).toMap

    val fToMaxCount = vocabulary.definitions.map(d => {
      (d, dtToFToCount.values.map(_(d)).max)
    }).toMap

    val singles = vocabulary.definitions.filter(d => fToMaxCount(d) > 0).map(d => {
      val params = d.getType.get.subtrees.dropRight(1)
      val singleVocab = SortedVocabulary(vocabulary.datatypes.filter(dt => params.contains(dt.asType)), Set(d))
      (singleVocab, fToMaxCount(d) + 1)
    }).toSet

    val couples = vocabulary.definitions.toSeq.combinations(2)
      .filter(ds => vocabulary.datatypes.exists(dt => dtToFToCount(dt)(ds(0)) > 1 && dtToFToCount(dt)(ds(1)) > 1))
      .map(ds => {
      (SortedVocabulary(vocabulary.datatypes, ds.toSet), ds.map(fToMaxCount).max + 1)
    }).toSet

//    Seq(singles, couples)
    Seq(singles)
  }

  /** Run tasks found by distributer parallely, by order and save results
    *
    * @param state Containing rewrite rules representing the different functions
    * @return Updated state with found rules
    */
  def runTasks(state: ActionSearchState): ActionSearchState = {
    val vocabs = getExplorationTasks
    for(vset <- vocabs) {
      val res = vset.par.map({
        case (v, i) =>
          val thesy = new TheoryExplorationAction(v, exampleDepth, None, None, None, None, Some(i), false)
          val resState = thesy(state.deepCopy())
          rules ++= thesy.getFoundRules
          resState
      }).seq
      for(s <- res) state.addRules(s.rewriteRules)
    }
    state
  }
}
