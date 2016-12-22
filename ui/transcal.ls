fs = require 'fs'

load = (fn) ->
  json = JSON.parse fs.readFileSync fn, 'utf-8'

  jsonToTree = (json) -> new Tree(json.root.literal, json.subtrees.map jsonToTree)

  prog: jsonToTree(json.program)
  elab: json.elaborate.map (.map jsonToTree)


draw = (state) ->
  pp = new PrettyPrint
  pp.pretty-print state.prog .append-to ($ '<p>' .append-to '#workbench')
  for el in state.elab
    $ '<p>' .append-to '#workbench'
      pp.pretty-print el.0 .append-to ..
      $ '<span>' .text " ⇢ " .append-to ..
      pp.pretty-print el.1 .append-to ..

$ ->
  draw load '../prog.json'

  $ '#reload' .click ->
    $ '#workbench' .empty!
    draw load '../prog.json'
