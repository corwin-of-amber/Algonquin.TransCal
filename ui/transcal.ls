fs = require 'fs'

load = (fn) ->
  TransCal.parse-output fs.readFileSync fn, 'utf-8'


pp = new PrettyPrint


draw = (state) ->
  #pp.pretty-print state.prog .append-to ($ '<p>' .append-to '#workbench')
  for el in state.elab
    $ '<p>' .append-to '#workbench'
      pp.pretty-print el.0 .append-to ..
      $ '<span>' .add-class 'elaborate-into' .text " ⇢ " .data 'annotations' el.2 .append-to ..
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
        $ '#debug-data' .text ast._id
    ..mouseleave ->
      $(@).parent!remove-class 'hover'
      <~ requestAnimationFrame
      $(@).parent!remove-attr 'draggable'
    ..click (ev) ->
      $(@).parent!trigger 'selected', {add: ev.metaKey}
      ev.stopPropagation!

  dom.find '.ast-leaf'
    ..mouseenter ->
      $(@).add-class 'hover'
      if (ast = $(@).data 'ast')?
        $ '#debug-data' .text ast._id
    ..mouseleave -> $(@).remove-class 'hover'

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

    dom.find '.ast-leaf'
      ..on \dragover ->
        it.preventDefault!
        $(@).add-class 'drag-hover'
      ..on \dragleave ->
        $(@).remove-class 'drag-hover'
      ..on \drop (ev) ->
        $(@).remove-class 'drag-hover'
        if ! ev.originalEvent.expression
          ev.stopPropagation!
          ev0 = new Event 'drop'
            ..dataTransfer = ev.originalEvent.dataTransfer
            ..expression = true
          @.dispatchEvent ev0

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


embrace = (target, caption, positioning='auto') ->
  target.closest 'p[mu-markup]' .first!
    /**/ console.assert ..length > 0 /**/
    markup = ..data 'markup'
    /**/ console.assert markup? /**/
    {st, ed} = markup.insert-markers target

    if positioning == 'auto'
      positioning = if st.parent!closest('mu').has-class('above') then 'above' else 'below'

    st.add-class positioning

    brace = $ '<mu-brace>' .attr from: st.attr('id'), to: ed.attr('id')
      ..add-class positioning
      ..append caption
      configure-selection ..
      configure-drag-and-drop ..

    markup.typeset brace
    markup.adjust-paragraph ..

display = (state) ->
  $ '#workbench'
    ..empty!
    draw state
    setup-markups ..
    configure-selection ..
    configure-drag-and-drop ..
    ..find '.elaborate-into' .each (i, el) -> elaboration-piece-in $(el)

find-origin = (ast, within) ->
  aux = (ast) ->
    root-id = ast._id
    els = within.filter ((i,x) -> $(x).data('ast')?._id == root-id) .first!
    if els.length == 0
      els = $([])
      for child in ast.subtrees then els .= add aux child
    els
  fragments = aux ast
  # now we need all fragments to belong to the same p[mu-markup].
  # arbitrarily choose the first
  p = fragments.closest('p[mu-markup]').first!
  if p.length
    fragments.filter((i,x) -> p.has(x).length > 0)
      /**/ console.assert ..length > 0 /**/
  else
    fragments  # fallback

elaboration-piece-in = (el, positioning) ->
  annot = el.data 'annotations'
  el.prevAll!filter ((i,x) -> $(x).data('ast')?) .first!
    if ..length > 0 && (ast = ..data('ast'))?
      p = el.closest('p[mu-markup]')
      # Look with right-hand sides
      within = p.prevAll!find('*').not($('.elaborate-into').prevAll!find('*').addBack!)
      target = find-origin(ast, within)
      if target.length > 0
        if !positioning
          positioning = | annot.some (is /above/)  =>  'above'
                        | annot.some (is /below/)  =>  'below'
        embrace target, $('<p>').append(el.nextAll!), positioning
        return true

$ ->
  sizes = localStorage.getItem('split-sizes')

  if sizes
    sizes = JSON.parse(sizes)
  else
    sizes = [50, 50]  # default sizes

  split = Split ['#editor.pane', '#workbench.pane'], do
    sizes: sizes
    gutterSize: 5
    snapOffset: 0
    elementStyle: (dimension, size, gutterSize) ->
        'flex-basis': 'calc(' + size + '% - ' + gutterSize + 'px)'
    gutterStyle: (dimension, gutterSize) ->
        'flex-basis':  gutterSize + 'px'
    onDragEnd: ->
      localStorage.setItem 'split-sizes', JSON.stringify(split.getSizes!);

$ ->
  TransCal.run fs.readFileSync "../examples/NoDup.tc", "utf-8"
  .then -> console.log "Done!" ; display it

  display load '../prog.json'

  $ '#workbench' .click -> $ '.selection' .remove-class 'selection'

  $ '#workbench' .on 'click' '.elaborate-into' (ev) ->
    elaboration-piece-in $(ev.target), ev.altKey && 'above'
      if !.. then console.warn "original term not found"

  #$ '#workbench .elaborate-into' .click!


$ ->
  $.contextMenu do
    selector: '#toggle-menu'
    trigger: 'left'
    items: $.contextMenu.fromMenu $ '#menu'
    animation: {duration: 0}
    position: (opt) ->
      opt.$menu
        ..position({ my: "right top", at: "right bottom", of: this, offset: "0 5"})


  $ '#reload' .click ->
    $ '#workbench' .empty!
    display load '../prog.json'

  $ '#helper-lines' .click ->
    $ '#workbench' .toggle-class 'helper-lines'


$ ->
  cm = new CodeMirror $('#editor').0, do
    line-numbers: true
  #  content: "text/coq"

  cm.setValue fs.readFileSync '../examples/NoDup.tc', 'utf-8'

