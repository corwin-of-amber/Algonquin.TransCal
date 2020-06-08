package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import synthesis.Programs
import transcallang.TranscalParser

import scala.collection.mutable

/**
  * BFS returns last state only.
  */
class SequentialSearch[S <: State[S], SS <: SearchSpace[S]] extends SearchDepth[S, SS, S] with LazyLogging {

  /* --- Search Impl. --- */

  def search(searchSpace: SS, maxDepth: Double): (Boolean, S) = {
    var state = searchSpace.initialStates.head
    state match {
      case state1: RewriteSearchState => logger.debug(s"Starting Naive Search. Graph size: ${state1.graph.size}")
      case _ =>
    }
    val operatorVer: mutable.Map[Operator[S], Long] = mutable.Map.empty
    var i = 0
    val parser = new TranscalParser
    val pattern = Programs.destructPattern(parser.parseExpression("zero ||| one"))

    var oldState: Option[S] = None
    while (i < maxDepth && !searchSpace.isGoal(state) && !oldState.contains(state)) {
      oldState = Some(state.deepCopy())
      if (state.asInstanceOf[RewriteSearchState].graph.findSubgraph[Int](pattern).nonEmpty)
        println("OMG")
      import scala.util.control.Breaks._
      breakable {
        for (op <- searchSpace.operators(state)) {
          state = op match {
            case versionedOp: VersionedOperator[S] =>
              val (s) = versionedOp.apply(state)
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
      logger.info(s"Done a round robin. Graph size is: ${state.asInstanceOf[RewriteSearchState].graph.size}")
    }

    (searchSpace.isGoal(state), state)
  }
}


