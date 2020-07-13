package ui

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import structures.mutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.search.ActionSearchState
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.{SortedVocabulary, TheoryExplorationAction}
import synthesis.search.rewrites.{PatternRewriteRule, RewriteRule}
import synthesis.search.rewrites.Template.TemplateTerm
import transcallang.{AnnotatedTree, Datatype, Identifier, Language}

class SmtlibInterperter {
  val state = new ActionSearchState(Programs.empty, Set.empty[RewriteRule])
  val datatypes = collection.mutable.Set.empty[Datatype]
  val knownFunctions = collection.mutable.Set.empty[AnnotatedTree]
  var goal: Option[(AnnotatedTree, AnnotatedTree)] = None

//  val ois = new ObjectInputStream(new FileInputStream("temp"))
//  var obj: AnyRef = null
//  var cont = true
//  while (cont) {
//    try {
//      obj = ois.readObject()
//    } catch {
//      case _: Throwable => cont = false
//    }
//    assert(obj != null)
//    obj match {
//      case x: AnnotatedTree => println(s"${Programs.termToString(x)}")
//    }
//  }

  def apply(terms: List[AnnotatedTree], oos: ObjectOutputStream) = {
    for(t <- terms) {
      t.root match {
        case Language.functionDeclId =>
          // This is a typed identifier for a function type
          knownFunctions += t.subtrees.head
        case Language.datatypeId =>
          // TODO: implement type parameters
          datatypes += Datatype(t.subtrees.head.root, Seq.empty, t.subtrees.tail.map(_.root))
        case Language.letId =>
          oos.writeObject(t)
          val letAction: LetAction = new LetAction(t, allowExistential = false)
          letAction(state)
        case Language.assertId =>
          oos.close()
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0), t.subtrees.head.subtrees(1)))
          val vocab = SortedVocabulary(datatypes.toSeq, knownFunctions.toSeq)
          val thesy = new TheoryExplorationAction(vocab, 3, None, None, None, None, None, true)
          thesy.addGoal(goal.get)
          thesy(state)
        case Identifier("not", annotation, namespace) if t.subtrees.head.root == Language.letId =>
          assert(t.subtrees.head.root == Language.letId)
          def cleanAutovar(identifier: Identifier) =
            if(identifier.literal.startsWith("?autovar")) identifier.copy(literal = identifier.literal.drop(1))
            else identifier
          goal = Some((t.subtrees.head.subtrees(0).map(cleanAutovar), t.subtrees.head.subtrees(1).map(cleanAutovar)))
          val vocab = SortedVocabulary(datatypes.toSeq, knownFunctions.toSeq)
          val thesy = new TheoryExplorationAction(vocab, 3, None, None, None, None, None, true)
          thesy.addGoal(goal.get)
          thesy(state)
          oos.writeObject(RunResults(datatypes.toSet, knownFunctions.toSet, thesy.getFoundRules, goal, thesy.goals.isEmpty))
        case x => throw new NotImplementedError("Check this")
      }
    }
  }
}
