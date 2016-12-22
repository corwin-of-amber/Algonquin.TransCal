{flatten} = require 'prelude-ls'

split-left = (ast, by-v=ast.root) ->
  if ast.root == by-v then (split-left ast.subtrees[0], by-v) ++ ast.subtrees[1 to]
  else [ast]

numeral = (ast) ->
  console.log ast
  if ast.is-leaf! && ast.root == "O" then 0
  else
    [fun, ...args] = split-left ast, "@"
    if fun.root == "S" && args.length == 1 && (n = numeral(args.0))? then n+1

cti = (text) -> document.createTextNode text

spaced = -> [cti(" "), it, cti(" ")]

infix-operators =
  plus:               {op: " + ", pri: 3, assoc: 'left'}
  eq:                 {op: " = ", pri: 4}
  lt:                 {op: " < ", pri: 4}
  gt:                 {op: " > ", pri: 4}
  le:                 {op: " ≤ ", pri: 4}
  ge:                 {op: " ≥ ", pri: 4}
  and:                {op: " ∧ ", pri: 7, assoc: 'left'}
  or:                 {op: " ∨ ", pri: 7, assoc: 'left'}
  mult: ([x,y]) ->
    if numeral(x)? then {op: "",  pri:-1, assoc: 'left'}
    else              {op: " ⋅ ", pri: 2, assoc: 'left'}
  prod:               {op: " × ", pri: 2}

  # From FRAP
  andb: ->            {op: [(cti " "), ($ '<span>' .addClass 'operator bool' .text "&&"), (cti " ")], pri: 2}
  orb:  ->            {op: [(cti " "), ($ '<span>' .addClass 'operator bool' .text "||"), (cti " ")], pri: 2}
  In:                 {op: " ⋵ ", pri: 4}
  lookup: ->          {op: [(cti " "), ($ '<span>' .addClass 'operator fmap' .text "?"), (cti " ")], pri: 2}


class Notation
  (@pri, @assoc) ->

  mkdom: (op) ->
    if !(op instanceof Node || op instanceof $ || op instanceof Array)
      cti op
    else if $.isFunction(op)
      op!

class InfixOperator extends Notation
  (@op, pri, assoc = \none) -> super(pri, assoc)

  format: (pp, term) ->
    console.assert(term.subtrees.length == 2)
    [left, right] = [term.subtrees[0], term.subtrees[1]]
    x =
      * pp.pretty-print left, @pri, (@assoc == 'left' || @assoc == 'both')
      * @mkdom @op
      * pp.pretty-print right, @pri, (@assoc == 'right' || @assoc == 'both')
    flatten x

class PrefixOperator extends Notation
  (@op, pri) -> super(pri, \right)

  format: (pp, term) ->
    console.assert(term.subtrees.length == 1)
    right = term.subtrees[0]
    x =
      * @mkdom @op
      * pp.pretty-print right, @pri, true
    flatten x

class Brackets extends Notation
  (@left, @right) -> super(0, \both)

  format: (pp, term) ->
    console.assert(term.subtrees.length == 1)
    element = term.subtrees[0]
    x =
      * @mkdom @left
      * pp.pretty-print element, @pri, true
      * @mkdom @right
    flatten x


notations =
  ':': new InfixOperator(" : ", 6, \right)
  '∧': new InfixOperator(" ∧ ", 7, \left)
  '↦': new InfixOperator(" ↦ ", 8, \right)
  '/': new InfixOperator(" / ", 9, \right)
  '∉': new InfixOperator(" ∉ ", 6, \none)
  '‖': new InfixOperator(" ‖ ", 6, \none)
  '∪': new InfixOperator(" ∪ ", 6, \left)
  '¬': new PrefixOperator("¬", 3)
  '{.}': new Brackets("{", "}")

aliases =
  'Init.Nat.add': 'plus'
  'Nat.add': 'plus'
  'PeanoNat.Nat.add': 'plus'
  'Init.Nat.mul': 'mult'

dealias = -> aliases[it] ? it

implicit-arguments =
  eq: [0]
  fold_left: [0,1]
  app: [0]
  cons: [0]
  pair: [0,1]
  Some: [0]

record-types = {}


class PrettyPrint

  #(@coq-options) ->
  ->

  load-metadata: (json) ->
    for k,v of json.vars ? {}
      if v.implicit?
        implicit-arguments[k] = v.implicit
    for k,v of json.recs ? {}
      record-types[k] = v


  pretty-print: (ast, pri=9, assoc=false) ->
    #console.log ast
    pri_ = void
    span =
      if (n = numeral(ast))?
        $ '<span>' .text n
      else if ast.root == "@"
        [fun, ...args] = split-left ast
        #if fun.is-leaf! && @coq-options.printing.implicit && (impt = implicit-arguments[dealias fun.root])?
        #  args = [x for x, i in args when i not in impt]
        $ '<span>'
          if (infix = infix-operators[dealias fun.root])?
            if $.isFunction(infix) then infix = infix args
            ..append <| @binop args.0, infix.op, args.1, (pri_ = infix.pri), infix.assoc
          else if (nota = notations[fun.root])?
            ..append <| nota.format @, new Tree(fun.root, args)
          else if fun.root == "pair" && args.length == 2
            pri_ = -1
            x =
              * cti "⟨"
              * @pretty-print args.0, 9, true
              * cti ", "
              * @pretty-print args.1, 9, true
              * cti "⟩"
            ..append flatten x
          else if fun.root == "list"
            pri_ = -1
            ..append cti "["
            ..append @pretty-print args.0, 9, true
            ..append cti "]"
          else if fun.root == "nil"
            ..append cti "[]"
          #else if fun.root == ":"
          #  op = [(cti " "), ($ '<span>' .addClass 'list-cons' .text ":"), (cti " ")]
          #  ..append <| @binop args.0, op, args.1, (pri_ = 2), 'right'
          else if fun.root == "app" && args.length == 2
            op = [(cti " "), ($ '<span>' .addClass 'list-app' .text "+"), (cti " ")]
            ..append <| @binop args.0, op, args.1, (pri_ = 3), 'right'
          else if fun.root == "length" or fun.root == "Datatypes.length"
            pri_ = -1
            ..append cti "|"
            ..append @pretty-print args.0, 9, true
            ..append cti "|"
          else if (mo = /^Build_(.*)/.exec fun.root)
            pri_ = -1
            ..add-class 'record'
            ..attr 'data-record-name', mo.1
            fields = $ '<table>'
            field-names = record-types[mo.1]?.fields ? []
            for s, i in args
              $ '<tr>' .append-to fields
                if (field-name = field-names[i])?
                  $ '<th>' .text field-name .append-to ..
                $ '<td>' .append @pretty-print s .append-to ..
            ..append fields
          else if fun.root == "sig" and args.0.root == "fun"
            pri_ = -1
            ..append cti "{"
            ..append @pretty-print args.0.subtrees.0, 9, true
            ..append cti " | "
            ..append @pretty-print args.0.subtrees.1, 9, true
            ..append cti "}"
          else
            pri_ = 0
            ..append @pretty-print fun, pri_, true
            for x, i in args
              ..append cti " "
              ..append @pretty-print x, pri_, false
      else if (nota = notations[ast.root])?
        $ '<span>'
          ..append <| nota.format @, ast
      else if ast.root == ":" && ast.subtrees.length > 1
        $ '<span>'
          pri_ = 5
          for va, i in ast.subtrees[til -1]
            if i > 0 then ..append document.createTextNode " "
            ..append @pretty-print va, pri_, true
          ..append document.createTextNode " : "
          ..append @pretty-print ast.subtrees[*-1]
      else if ast.root == "->"
        $ '<span>'
          pri_ = 4
          ..append @pretty-print ast.subtrees.0, pri_, true
          ..append document.createTextNode " → "
          ..append @pretty-print ast.subtrees.1, pri_, true
      else if ast.root == "forall" || ast.root == "fun"
        $ '<span>'
          pri_ = 5
          ..append do
            switch ast.root
            | "forall" => $ '<span>' .addClass 'forall quantifier' .text "∀"
            | "fun"    => $ '<span>' .addClass 'fun' .text "λ"
          lassoc = ast.subtrees.length <= 2
          for va, i in ast.subtrees[til -1]
            if i > 0 then ..append document.createTextNode " "
            ..append @pretty-print va, pri_, lassoc
          ..append document.createTextNode ", "
          ..append @pretty-print ast.subtrees[*-1], pri_, true
      else if ast.root == "match"
        $ '<span>' .add-class 'match' .text "match"
          cases = $ '<table>'
          for s in ast.subtrees
            if s.root == "=>"
              $ '<tr>' .append-to cases
                $ '<th>' .append @pretty-print s.subtrees[0] .append-to ..
                $ '<td>' .append @pretty-print s.subtrees[1] .append-to ..
            else ..append [cti(" "), @pretty-print s]
          ..append cases
      else if ast.root == "return"
        $ '<span>' .add-class 'return' .text "⇢ "
          ..append @pretty-print ast.subtrees[0]
      else if ast.root == ":="
        $ '<span>' 
          pri_ = 5
          st = if ast.subtrees[0].root == "return" then 1 else 0
          for va, i in ast.subtrees[st til -1]
            if i > 0 then ..append document.createTextNode " "
            ..append @pretty-print va, pri_, false
          ..append document.createTextNode " ↦ "
          ..append @pretty-print ast.subtrees[*-1], pri_, true
      else if ast.root == "fix"
        pri_ = 9
        $ '<span>' .add-class 'fix' .text "fix "
          for va, i in ast.subtrees
            if i > 0 then ..append document.createTextNode " "
            ..append @pretty-print va
      else if ast.is-leaf!
        if ast.root == "nat"
          $ '<span>' .add-class 'type-nat' .text "N"
        else if ast.root == "true" || ast.root == "false"
          $ '<span>' .add-class 'const bool' .text ast.root
        else if ast.root == "True"
          $ '<span>' .add-class 'const Prop' .text "⊤"
        else if ast.root == "False"
          $ '<span>' .add-class 'const Prop' .text "⊥"
        else if ast.root == "nil"
          $ '<span>' .text "[]"
        else if (mo = /^[?](.*)$/.exec ast.root)
          $ '<span>' .add-class 'metavar' .text mo.1
        else
          $ '<span>' .text ast
      else
        $ '<span>' .text (ast.root + "{")
          for x,i  in ast.subtrees
            if i then ..append document.createTextNode ", "
            ..append @pretty-print x
          ..append ($ '<span>' .text ("}"))

    span
      if pri_? && (pri_ > pri || (pri_ == pri && !assoc))
        ..prepend document.createTextNode "("
        ..append document.createTextNode ")"

  binop: (left, op, right, pri, assoc = 'none') ->
    if !(op instanceof Node || op instanceof $ || op instanceof Array)
      op = cti op
    else if $.isFunction(op)
      op = op!
    x =
      * @pretty-print left, pri, (assoc == 'left' || assoc == 'both')
      * op
      * @pretty-print right, pri, (assoc == 'right' || assoc == 'both')
    flatten x

  display-sequent: (sequent) ->
    div = $ '<div>' .add-class 'sequent'
    if sequent.env? && sequent.env.length > 0
      table = $ '<table>' .add-class 'env' .append-to div
      for hyp in sequent.env
        tr = $ '<tr>' .add-class 'premise' .append-to table
        if hyp.root == ':'
          $ '<th>' .add-class 'name' .append-to tr
            hyp.subtrees[til -1].for-each ~> ..append [cti(" "), @pretty-print sugar it]
          $ '<td>' .add-class 'type' .append @pretty-print sugar hyp.subtrees[*-1] .append-to tr
        else
          $ '<th>' .append-to tr
          $ '<td>' .append @pretty-print hyp .append-to tr
    if sequent.goal?
      $ '<p>' .add-class 'goal' .append @pretty-print sugar sequent.goal .append-to div
    div

/**
 * This is supposed to re-sugar some notations such as "->".
 * Should be done more systematically.
 */
sugar = (ast) ->
  ast.subtrees .= map sugar

  while ast.root == "forall" &&
        ast.subtrees[*-2].root == ':' &&
        ast.subtrees[*-2].subtrees.length == 2 &&
        ast.subtrees[*-2].subtrees[0].root == '_'
    ast.subtrees = ast.subtrees[til -2] ++ [T("->", [ast.subtrees[*-2].subtrees[1], ast.subtrees[*-1]])]
    if ast.subtrees.length == 1
      ast = ast.subtrees.0
      break

  ast




@ <<< {notations, PrettyPrint}
