

$ ->
  offy = 0
  boxes = []
  $ 'mu-brace' .each (i, brace) ->
    st = $ 'mu#' + $(brace).attr('from')
    ed = $ 'mu#' + $(brace).attr('to')
    w = ed.position!left - st.position!left + 1
    top = lowest(boxes, [st.offset!left, ed.offset!left])
    spc = Math.max(0, top - st.offset!top)

    t = $ '<table>'
      for a in brace.attributes then ..attr a.name, a.value
      if spc > 0
        ..append ($('<tr>').append($ '<td>' .add-class 'spacer' .height spc))
      ..append ($('<tr>').append($ '<td>' .add-class 'brace'))
      ..append ($('<tr>').append(capt = $ '<td>' .add-class 'caption' .append $(brace).remove!contents!))
      ..width w
      st.append ..

    boxes.push capt


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
