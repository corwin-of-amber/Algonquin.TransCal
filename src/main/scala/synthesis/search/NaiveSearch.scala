package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import synthesis.rewrites.RewriteSearchState

import scala.collection.mutable

/**
  * BFS returns last state only.
  */
class NaiveSearch[S <: State[S], SS <: SearchSpace[S]] extends SearchDepth[S, SS, S] with LazyLogging {

  /* --- Search Impl. --- */

  def search(searchSpace: SS, maxDepth: Double): (Boolean, S) = {
    var state = searchSpace.initialStates.head
    state match {
      case state1: RewriteSearchState => logger.debug(s"Starting Naive Search. Graph size: ${state1.graph.size}")
      case _ =>
    }
    val operatorVer: mutable.Map[Operator[S], Long] = mutable.Map.empty
    var i = 0

    var oldState: Option[S] = None
    while (i < maxDepth && !searchSpace.isGoal(state) && !oldState.contains(state)) {
      oldState = Some(state.deepCopy())
      import scala.util.control.Breaks._
      breakable {
        for (op <- searchSpace.operators(state)) {
          state = op match {
            case versionedOp: VersionedOperator[S] =>
              val version = operatorVer.getOrElse(versionedOp, 0L)
              val (s, newVer) = versionedOp.apply(state, 0)
              operatorVer.put(versionedOp, newVer)
              s
            case _ =>
              logger.warn(s"Using a non versioned operator $op")
              op(state)
          }
          if (searchSpace.isGoal(state)) {
            logger.debug("Found goal. Stopping NaiveSearch")
            break
          }
        }
      }
      i += 1
      logger.debug(s"Done a round robin. Graph size is: ${state.asInstanceOf[RewriteSearchState].graph.size}")
    }

    (searchSpace.isGoal(state), state)
  }
}


