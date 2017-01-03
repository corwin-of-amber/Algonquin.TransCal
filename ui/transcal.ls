fs = require 'fs'

load = (fn) ->
  json = JSON.parse fs.readFileSync fn, 'utf-8'

  jsonToTree = (json) ->
    new Tree(json.root.literal, json.subtrees.map jsonToTree)
      .._id = json._id

  prog: jsonToTree(json.program)
  elab: json.elaborate.map (.map jsonToTree)


pp = new PrettyPrint


draw = (state) ->
  pp.pretty-print state.prog .append-to ($ '<p>' .append-to '#workbench')
  for el in state.elab
    $ '<p>' .append-to '#workbench'
      pp.pretty-print el.0 .append-to ..
      $ '<span>' .add-class 'elaborate-into' .text " ⇢ " .append-to ..
      pp.pretty-print el.1 .append-to ..

setup-markups = (dom) ->
  dom.children 'p' .each (i, p) ->
    $ p
      if ! ..attr 'mu-markup'
        ..attr 'mu-markup' ''
        ..data markup: new Markup

configure-selection = (dom) ->
  dom.find '.ast-root'
    ..mouseenter ->
      $(@).parent!add-class 'hover'
      $(@).parent!attr 'draggable' true
      if (ast = $(@).parent!data 'ast')?
        $ 'button' .text ast._id
    ..mouseleave ->
      $(@).parent!remove-class 'hover'
      $(@).parent!remove-attr 'draggable'
    ..click (ev) ->
      $(@).parent!trigger 'selected', {add: ev.metaKey}
      ev.stopPropagation!

  dom.find 'span'
    ..on \selected (ev, opts={}) ->
      ev.stopPropagation!
      container = if opts.add then $(@) else $ '#workbench'
      container.find '.selection' .remove-class 'selection'
      if container.closest '.selection' .length == 0
        $(@).add-class 'selection'


configure-drag-and-drop = do ->
  dragged = void
  (dom) ->
    dom.find '.ast-root'

      ..on \dragover ->
        it.preventDefault!
        $(@).parent!add-class 'drag-hover'
      ..on \dragleave ->
        $(@).parent!remove-class 'drag-hover'
      ..on \drop (ev) ->
        $(@).parent!remove-class 'drag-hover'
        $(@).parent!add-class 'hover'
        ev.stopPropagation!
        ev0 = new Event 'drop'
          ..dataTransfer = ev.originalEvent.dataTransfer
          ..expression = true
        $(@).parent!0.dispatchEvent ev0

    dom.find 'span'
      ..on \dragstart (ev) ->
        dragged := ev.target
        ev.stopPropagation!
      ..on \dragend (ev) ->
        $(ev.target).remove-class 'hover'
        dragged := void
        ev.stopPropagation!

      # On drop expression, create a brace
      ..on \drop (ev) ->
        ev.stopPropagation!
        if ev.originalEvent.expression
          if (ast = $(dragged).data 'ast')?
            console.log 'drop!', ast
            console.log 'selected', ev.target

            drop-target =
              if $(ev.target).has-class 'selection' then $ '.selection'
              else $(ev.target)
            caption = $ '<p>' .append pp.pretty-print ast

            embrace drop-target, caption


embrace = (target, caption) ->
  target.closest 'p[mu-markup]' .first!
    /**/ console.assert ..length > 0 /**/
    markup = ..data 'markup'
    /**/ console.assert markup? /**/
    {st, ed} = markup.insert-markers target

    brace = $ '<mu-brace>' .attr from: st.attr('id'), to: ed.attr('id')
      ..append caption
      configure-selection ..
      configure-drag-and-drop ..

    markup.typeset brace
    markup.adjust-paragraph ..

display = (state) ->
  draw state
  $ '#workbench'
    setup-markups ..
    configure-selection ..
    configure-drag-and-drop ..

find-origin = (ast, within) ->
  root-id = ast._id
  console.log root-id
  els = within.filter ((i,x) -> $(x).data('ast')?._id == root-id)
  if els.length == 0
    els = $([])
    for child in ast.subtrees then els .= add find-origin(child, within)
  els

$ ->
  display load '../prog.json'

  $ '#reload' .click ->
    $ '#workbench' .empty!
    display load '../prog.json'


  $ '#workbench' .click -> $ '.selection' .remove-class 'selection'


  $ '#workbench' .click '.elaborate-into' (ev) ->
    $(ev.target).prevAll!filter ((i,x) -> $(x).data('ast')?) .first!
      if ..length > 0 && (ast = ..data('ast'))?
        console.log ast._id
        p = $(ev.target).closest('p[mu-markup]')
        within = p.prevAll!find('*').not($('.elaborate-into').prevAll!find('*'))
        console.log within
        target = find-origin(ast, within)
        console.log target
        embrace target, $('<p>').append $(ev.target).nextAll!
