package relentless.rewriting

import syntax.Tree
import syntax.Identifier
import syntax.AstSugar._
import relentless.matching.Trie
import relentless.matching.Encoding


/**
 * Reconstruction of terms from tuples stored in the trie.
 */
class Reconstruct private (init: Tree[Int], trie: Trie[Int]) {
  
  import collection.mutable
  import math.Ordering
  import Reconstruct._

  def this(root: Int, trie: Trie[Int]) = this(new Tree(root), Reconstruct.mkTrie1(trie.words))
  def this(root: Int, words: Seq[Array[Int]]) = this(new Tree(root), Reconstruct.mkTrie1(words))
  def this(tuple: Array[Int], trie: Trie[Int]) = this(Reconstruct.tupleToTree(tuple), Reconstruct.mkTrie1(trie.words))
  
  case class Entry(val t: Tree[Int]) {
    val pri = -t.size
  }
      
  val pq = new mutable.PriorityQueue[Entry]()(Ordering.by(_.pri)) += Entry(init)
  val ws = mutable.Set.empty[Tree[Int]]

  def apply() = {
    whileYield (!pq.isEmpty, {
      val e = pq.dequeue()
      var expanded = false
      for (leaf <- e.t.leaves if !expanded;
           alt <- trie.get(1, leaf.leaf) map (_.words) getOrElse List()) {
        val expand = e.t.replaceDescendant((leaf, tupleToTree(alt)))
        expanded = true
        if (ws add expand)
          pq enqueue Entry(expand)
      }
      if (expanded) None else Some(e.t)
    }) flatten
  }
  
  def apply(enc: Encoding): Stream[Term] = {
    for (t <- apply()) yield decode(t)(enc)
  }
  
  def decode(t: Tree[Int])(implicit enc: Encoding): Term = {
    //println(enc.ntor <-- t.root)
    enc.ntor <-- t.root match {
      case r: Identifier =>
        r.kind match {
          case "operator" | "connective" | "quantifier" => T(r)(t.subtrees map decode)
          case _ => T(r):@(t.subtrees map decode)
        }
      case t: Tree[_] => t.asInstanceOf[Term] // hope there are no other trees
    }
  }
}

object Reconstruct {
  
  /** Build a trie indexed by location 1 */
  def mkTrie1(words: Seq[Array[Int]]) = {
    val t = new Trie[Int](new Tree(-1, List(new Tree(1))))
    words foreach t.add
    t
  }
  
  def tupleToTree(tuple: Array[Int]) = new Tree(tuple(0), tuple drop 2 map (new Tree(_)) toList)
  
  def onlyIf[A](cond: Boolean, op: => Seq[A]) = if (cond) op else Seq.empty
  def whileYield[A](cond: => Boolean, vals: => A) = takeWhileLeft(cond, Iterator.continually(vals))
  def takeWhileLeft[A](cond: => Boolean, it: Iterator[A]): Stream[A] =
    if (!cond) Stream.empty
    else it.next #:: takeWhileLeft(cond, it)
  
}
  