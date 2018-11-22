package synthesis.search

import scala.collection.mutable

/**
  * BFS returns last state only.
  */
class BreadthFirstSearch[S <: State, SS <: SearchSpace[S]] extends SearchDepth[S, SS, S] {

  /* --- Search Impl. --- */

  def search(searchSpace: SS, maxDepth: Double): Option[S] = {
    // a FIFO open_set
    val openQueue = mutable.Queue[BreadthFirstSearch.Node[S]]()
    // an empty set to maintain visited nodes
    val closedMap = mutable.HashMap[S, BreadthFirstSearch.Node[S]]()

    // initialize
    for (root <- searchSpace.initialStates) {
      openQueue.enqueue(BreadthFirstSearch.Node[S](root, maxDepth))
    }

    //For each node on the current level expand and process, if no children (leaf, then unwind)
    while (openQueue.nonEmpty) {
      val current = openQueue.dequeue()

      //We found the node we wanted so stop and emit a path.
      if (searchSpace.isGoal(current.state)) {
        return Some(current.state)
      }

      if (current.depth > 0) {
        //We finished processing the root of this subtree, so add it to the closed
        closedMap.put(current.state, current)
        //For each child of the current tree process
        for (newState <- searchSpace.neighbors(current.state)) {
          //The node has already been processed, so skip over it
          if (!closedMap.contains(newState)) {
            //The child is not enqueued to be processed, so enqueue this level of children to be expanded
            if (!openQueue.exists(p => p.state == newState)) {
              //enqueue these nodes
              openQueue.enqueue(BreadthFirstSearch.Node[S](newState, current))
            }
          }
        }
      }
    }
    None
  }
}

object BreadthFirstSearch {
  private class Node[S <: State](val state: S, val parent: Option[Node[S]], val depth: Double) {

    /**
      * Produce a backtrace of the actions taken to find the goal node, using the recorded meta dictionary
      */
    def constructPath(first: Node[S]): IndexedSeq[S] = {
      val total_path = mutable.MutableList[S]()
      var current = first
      total_path += current.state

      // Continue until you reach root
      while (current.parent.isDefined) {
        current = current.parent.get
        total_path += current.state

      }
      total_path.reverse.toIndexedSeq
    }
  }
  private object Node {
    def apply[S <: State](state: S, parent: Option[Node[S]], depth: Double): Node[S] = new Node[S](state, parent, depth)

    def apply[S <: State](state: S, parent: Node[S]): Node[S] = new Node[S](state, Some(parent), parent.depth - 1)

    def apply[S <: State](state: S, depth: Double): Node[S] = new Node[S](state, None, depth)
  }
}
