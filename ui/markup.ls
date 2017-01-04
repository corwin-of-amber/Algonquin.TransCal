

$ ->
  markup = new Markup
  markup.typeset($ 'mu-brace' .remove!)


class Markup

  ->
    @boxes = []

  typeset: (braces) ->
    braces.each (i, brace) ~>
      st = $ 'mu#' + $(brace).attr('from')
      ed = $ 'mu#' + $(brace).attr('to')
      dir = if st.has-class 'above' then 'up' else 'down'
      w = ed.offset!left - st.offset!left + 1
      if dir == 'down'
        top = lowest(@boxes, [st.offset!left, ed.offset!left])
        spc = Math.max(0, top - st.offset!top)
      else
        bottom = highest(@boxes, [st.offset!left, ed.offset!left])
        spc = Math.max(0, st.offset!top - bottom - 1)

      t = $ '<table>'
        for a in brace.attributes then ..attr a.name, a.value
        if dir == 'down'
          if spc > 0
            ..append ($('<tr>').append($ '<td>' .add-class 'spacer' .height spc))
          ..append ($('<tr>').append($ '<td>' .add-class 'brace'))
          ..append ($('<tr>').append(capt = $ '<td>' .add-class 'caption' .append $(brace).contents!))
        else  /* dir == 'up' */
          ..append ($('<tr>').append(capt = $ '<td>' .add-class 'caption' .append $(brace).contents!))
          ..append ($('<tr>').append($ '<td>' .add-class 'brace'))
          if spc > 0
            ..append ($('<tr>').append($ '<td>' .add-class 'spacer' .height spc))
          capt.css 'bottom', 7 /* = height of brace */ + spc
        if (sx = ..attr('shift-x')) then capt.css 'left', sx
        ..width w
        st.append ..

      @boxes.push capt

  insert-markers: (elements) ->
    console.assert elements.length > 0
    max = Math.max 0, ...($ 'mu' .map (i,x) -> x.id)
    st: $ '<mu>' .attr id: max+1 .insert-before elements[0]
    ed: $ '<mu>' .attr id: max+2 .insert-after elements[*-1]

  adjust-paragraph: (p) ->
    p
      bottom = Math.max ...(..find 'td' .map (i, x) -> x.getBoundingClientRect!bottom)
      ..css margin-bottom: 17 + bottom - ..0.getBoundingClientRect!bottom
      top = Math.min ...(..find 'td' .map (i, x) -> x.getBoundingClientRect!top)
      ..css margin-top: 17 + Math.max(0, ..0.getBoundingClientRect!top - top)

  #for ops then ..!
  #ops[0]!
  #setTimeout ->
  #  for op in ops[1 to] then op!
  #, 0


lowest = (boxes, [st-x, ed-x]) ->
  bottoms =
    for box in boxes
      rect = box.0.getBoundingClientRect!
        [l, r] = [..left, ..right]
      if (l <= st-x <= r) ||
         (l <= ed-x <= r) ||
         (st-x < l && r < ed-x)
      then rect.bottom
  Math.max ...bottoms

highest = (boxes, [st-x, ed-x]) ->
  tops =
    for box in boxes
      rect = box.0.getBoundingClientRect!
        [l, r] = [..left, ..right]
      if (l <= st-x <= r) ||
         (l <= ed-x <= r) ||
         (st-x < l && r < ed-x)
      then rect.top
  Math.min ...tops


export Markup
