package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import synthesis.rewrites.RewriteSearchState

import scala.collection.mutable

/**
  * BFS returns last state only.
  */
class NaiveSearch[S <: State, SS <: SearchSpace[S]] extends SearchDepth[S, SS, S] with LazyLogging {

  /* --- Search Impl. --- */

  def search(searchSpace: SS, maxDepth: Double): Option[S] = {
    var state = searchSpace.initialStates.head
    val operatorVer: mutable.Map[Operator[S], Long] = mutable.Map.empty
    var i = 0

    while (i < maxDepth && !searchSpace.isGoal(state)) {
      val op = searchSpace.operators(state).drop(i % searchSpace.operators(state).size).head
      state = op match {
        case versionedOp: VersionedOperator[S] =>
          val version = operatorVer.getOrElse(versionedOp, 0l)
          val (s, newVer) = versionedOp.apply(state, 0)
          operatorVer.put(versionedOp, newVer)
          s
        case _ =>
          logger.warn(s"Using a non versioned operator $op")
          op(state)
      }
      i += 1
      if (i % searchSpace.operators(state).size == 0 && state.isInstanceOf[RewriteSearchState])
        logger.debug(s"Done a round robin. Graph size is: ${state.asInstanceOf[RewriteSearchState].graph.size}")
    }

    Some(state).filter(searchSpace.isGoal)
  }
}


