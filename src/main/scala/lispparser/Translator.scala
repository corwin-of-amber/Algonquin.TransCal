package lispparser

import java.io.{Reader, StringReader}

import smtlib.trees.{Commands, CommandsResponses, Terms}
import smtlib.trees.Commands.Command
import smtlib.trees.Terms.{SExpr, SSymbol, Sort, SortedVar, Term}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.collection.mutable.ListBuffer

class Translator(text: String) {
  val lexer = new smtlib.lexer.Lexer(new StringReader(text))
  val parser = new smtlib.parser.Parser(lexer)
  val smtlibScript: List[Command] = {
    var cmds = new ListBuffer[Command]
    var cmd = parser.parseCommand
    while (cmd != null) {
      cmds.append(cmd)
      cmd = parser.parseCommand
    }
    cmds.toList
  }

  val identifierGenerator: Iterator[Identifier] = Stream.from(0).map(i => Identifier(s"autovar$i")).toIterator

  def transSSymbol(symbol: SSymbol): AnnotatedTree = AnnotatedTree.identifierOnly(Identifier(symbol.name))

  def transSExp(sexp: SExpr): AnnotatedTree = sexp match {
    case value: Terms.AttributeValue => throw new NotImplementedError()
    case Terms.SList(sexprs) =>
      val subExps = sexprs.map(transSExp)
      if (subExps.isEmpty) AnnotatedTree.identifierOnly(Language.tupleId)
//      else if (subExps.head.root.literal == "ite")
//        AnnotatedTree.withoutAnnotations(Language.matchId, List(
//          subExps(1),
//          AnnotatedTree.withoutAnnotations(Language.guardedId, List(AnnotatedTree.identifierOnly(Language.trueId), subExps(2))),
//          AnnotatedTree.withoutAnnotations(Language.guardedId, List(AnnotatedTree.identifierOnly(Language.falseId), subExps(3)))
//        ))
      else if (subExps.head.subtrees.isEmpty) AnnotatedTree.withoutAnnotations(subExps.head.root, subExps.tail)
      else AnnotatedTree.withoutAnnotations(Language.applyId, subExps)
    case Terms.SKeyword(name) => AnnotatedTree.identifierOnly(Identifier(name))
    case a@SSymbol(name) => transSSymbol(a)
    case term: Term => transTerm(term)
    case command: Command => throw new NotImplementedError()
    case response: CommandsResponses.CommandResponse => throw new NotImplementedError()
  }

  def transIdentifier(ident: Terms.Identifier): Identifier = {
    def translateName(name: String) = name match {
      case "=>" => "implication"
      case x => x
    }
    Identifier(translateName(ident.symbol.name) + ident.indices.mkString("_"))
  }

  def transSort(sort: Sort): AnnotatedTree = AnnotatedTree.withoutAnnotations(transIdentifier(sort.id), sort.subSorts.map(transSort))

  def transSortedVar(sortedVar: SortedVar): Identifier = transSSymbol(sortedVar.name).root.copy(annotation = Some(transSort(sortedVar.sort)))

  def fixEqualityId(annotatedTree: AnnotatedTree): AnnotatedTree = {
    if (annotatedTree.root == Language.letId)
      annotatedTree.copy(subtrees = annotatedTree.subtrees.map(t =>
        t.map(i => if(i== Language.letId) Language.equalityId else i)))
    else if (annotatedTree.root.literal == "not" && annotatedTree.subtrees.size == 1 && annotatedTree.subtrees.head.root == Language.letId)
      annotatedTree.copy(subtrees = List(annotatedTree.subtrees.head.copy(subtrees = annotatedTree.subtrees.head.subtrees.map(t =>
        t.map(i => if(i== Language.letId) Language.equalityId else i)))))
    else annotatedTree.map(i => if(i== Language.letId) Language.equalityId else i)
  }

  def transTerm(term: Term): AnnotatedTree = term match {
    case Terms.Let(binding, bindings, term) =>
      transTerm(term).replaceDescendants((binding +: bindings).map(b => (transSSymbol(b.name), transTerm(b.term))))
    case Terms.Forall(sortedVar, sortedVars, term) =>
      transTerm(term).replaceDescendants((sortedVar +: sortedVars).map(s => {
        val newIdent = identifierGenerator.next()
        (transSSymbol(s.name),
          AnnotatedTree.identifierOnly(newIdent.copy(literal = s"?${newIdent.literal}", annotation = Some(transSort(s.sort)))))
      }))
    case Terms.Exists(sortedVar, sortedVars, term) =>
      transTerm(term).replaceDescendants((sortedVar +: sortedVars).map(s => {
        val newIdent = identifierGenerator.next()
        (transSSymbol(s.name), AnnotatedTree.identifierOnly(newIdent.copy(annotation = Some(transSort(s.sort)))))
      }))
    case Terms.QualifiedIdentifier(id, sort) =>
      AnnotatedTree.identifierOnly(transIdentifier(id).copy(annotation = sort.map(transSort)))
    case Terms.AnnotatedTerm(term, attribute, attributes) => throw new NotImplementedError()
    case Terms.FunctionApplication(fun, terms) => AnnotatedTree.withoutAnnotations(transTerm(fun).root, terms.map(transTerm))
    case Terms.SNumeral(value) => AnnotatedTree.identifierOnly(Identifier(value.toString))
    case Terms.SHexadecimal(value) => AnnotatedTree.identifierOnly(Identifier(value.toInt.toString))
    case Terms.SBinary(value) => throw new NotImplementedError()
    case Terms.SDecimal(value) => throw new NotImplementedError()
    case Terms.SString(value) => AnnotatedTree.withoutAnnotations(Language.stringLiteralId, List(AnnotatedTree.identifierOnly(Identifier(value))))
    case c: Terms.Constant => throw new NotImplementedError()
    case _: Terms.Literal[_] => throw new NotImplementedError()
    case _: Terms.TermExtension => throw new NotImplementedError()
  }

  def transFunDecl(name: SSymbol, paramSorts: Seq[Sort], returnSort: Sort): AnnotatedTree = {
    val tcName = transSSymbol(name)
    AnnotatedTree.withoutAnnotations(Language.functionDeclId, List(tcName.copy(root = tcName.root.copy(annotation = Some(
      AnnotatedTree.withoutAnnotations(Language.mapTypeId, (paramSorts :+ returnSort).map(transSort))
    )))))
  }

  val transcalScript: List[AnnotatedTree] = smtlibScript.flatMap({
    case Commands.Assert(term) =>
      Seq(fixEqualityId(transTerm(term)))
    case Commands.CheckSat() => Seq.empty
    case Commands.CheckSatAssuming(propLiterals) => throw new NotImplementedError()
    case Commands.DeclareConst(name, sort) => throw new NotImplementedError()
    case Commands.DeclareFun(name, paramSorts, returnSort) => Seq(transFunDecl(name, paramSorts, returnSort))
    case Commands.DeclareSort(name, arity) => throw new NotImplementedError()
    case Commands.DefineFun(funDef) =>
      val params = funDef.params.map(transSortedVar).map(AnnotatedTree.identifierOnly)
      val replacers = params.map(m => {
        val newIdent = identifierGenerator.next()
        m.copy(root = m.root.copy(literal = "?" + newIdent.literal))
      })
      val tree = AnnotatedTree.withoutAnnotations(Language.letId, List(
        AnnotatedTree.withoutAnnotations(transSSymbol(funDef.name).root, replacers),
        transTerm(funDef.body).replaceDescendants(params.zip(replacers))
          .map(i => if(i== Language.letId) Language.equalityId else i)
      ))
      Seq(transFunDecl(funDef.name, funDef.params.map(_.sort), funDef.returnSort),
        tree)
    case Commands.DefineFunRec(funDef) =>
      val params = funDef.params.map(transSortedVar).map(AnnotatedTree.identifierOnly)
      val replacers = params.map(m => {
        val newIdent = identifierGenerator.next()
        m.copy(root = m.root.copy(literal = "?" + newIdent.literal))
      })
      val tree = AnnotatedTree.withoutAnnotations(Language.letId, List(
        AnnotatedTree.withoutAnnotations(transSSymbol(funDef.name).root, replacers),
        transTerm(funDef.body).replaceDescendants(params.zip(replacers))
          .map(i => if(i== Language.letId) Language.equalityId else i)
      ))
      Seq(transFunDecl(funDef.name, funDef.params.map(_.sort), funDef.returnSort),
        tree)
    case Commands.DefineFunsRec(funDecls, bodies) => throw new NotImplementedError()
    case Commands.DefineSort(name, params, sort) => throw new NotImplementedError()
    case Commands.Echo(value) => throw new NotImplementedError()
    case Commands.Exit() => throw new NotImplementedError()
    case Commands.GetAssertions() => throw new NotImplementedError()
    case Commands.GetAssignment() => throw new NotImplementedError()
    case Commands.GetInfo(flag) => throw new NotImplementedError()
    case Commands.GetModel() => throw new NotImplementedError()
    case Commands.GetOption(key) => throw new NotImplementedError()
    case Commands.GetProof() => throw new NotImplementedError()
    case Commands.GetUnsatAssumptions() => throw new NotImplementedError()
    case Commands.GetUnsatCore() => throw new NotImplementedError()
    case Commands.GetValue(term, terms) => throw new NotImplementedError()
    case Commands.Pop(n) => throw new NotImplementedError()
    case Commands.Push(n) => throw new NotImplementedError()
    case Commands.Reset() => throw new NotImplementedError()
    case Commands.ResetAssertions() => throw new NotImplementedError()
    case Commands.SetInfo(attribute) => throw new NotImplementedError()
    case Commands.SetLogic(logic) => throw new NotImplementedError()
    case Commands.SetOption(option) => throw new NotImplementedError()
    case extension: Commands.CommandExtension => extension match {
      case Commands.DeclareDatatypes(datatypes) => datatypes.map(d => {
        AnnotatedTree.withoutAnnotations(Language.datatypeId, transSSymbol(d._1) +: d._2.map(c => {
          val funType = {
            val params = c.fields.map(f => transSort(f._2))
            val datatype = transSSymbol(d._1)
            if (params.nonEmpty) AnnotatedTree.withoutAnnotations(Language.mapTypeId, params :+ datatype)
            else datatype
          }
          AnnotatedTree.identifierOnly(transSSymbol(c.sym).root.copy(annotation = Some(funType)))
        }))
      })
      case _ => throw new NotImplementedError()
    }
  })
}
