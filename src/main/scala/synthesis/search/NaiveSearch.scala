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
    state match {
      case state1: RewriteSearchState => logger.debug(s"Starting Naive Search. Graph size: ${state1.graph.size}")
      case _ =>
    }
    val operatorVer: mutable.Map[Operator[S], Long] = mutable.Map.empty
    var i = 0

    var oldState: Option[S] = Option.empty
    while (!oldState.contains(state) && i < maxDepth && !searchSpace.isGoal(state)) {
      oldState = Some(state)
      import scala.util.control.Breaks._
      breakable {
        for (op <- searchSpace.operators(state)) {
          state = op match {
            case versionedOp: VersionedOperator[S] =>
              val version = operatorVer.getOrElse(versionedOp, 0l)
              val (s, newVer) = versionedOp.apply(state, version)
              operatorVer.put(versionedOp, newVer)
              s
            case _ =>
              logger.warn(s"Using a non versioned operator $op")
              op(state)
          }
          i += 1
          if (searchSpace.isGoal(state)) {
            break
          }
        }
      }
        logger.debug(s"Done a round robin. Graph size is: ${state.asInstanceOf[RewriteSearchState].graph.size}")
    }

    Some(state).filter(searchSpace.isGoal)
  }
}


