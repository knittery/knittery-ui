###
  Draws stitches on a 2D-Canvas (resp. a context)
###
define([], () ->
  reduceToKnitted = (knitting) ->
    first = 200
    last = 0
    rows = for row, i in knitting
      for stitch, j in row
        if not (stitch.type == "no" or stitch.type == "empty")
          first = Math.min(first, j)
          last = Math.max(last, j)
      {index: i, data: row}
    last = Math.max(first, last)

    rows = _.reject(rows, (r) -> emptyRow(r.data))
    rows = _.reduce(rows, (m, r) ->
      if m.length == 0 then [r]
      else if noOverlap(_.last(m).data, r.data)
        r2 = m.pop()
        merged = {index: r.index, data: mergeRows(r2.data, r.data)}
        m.push(merged)
        m
      else
        m.push(r)
        m
    , [])
    row.data = row.data.slice(first, last + 1) for row in rows

    result =
      rows: rows
      stitchOffset: first
      stitches: last - first + 1
      originalRowToRow: (index) ->
        i = 0
        for r, i in @rows when r.index > index
          return i - 1
        return @rows.length - 1
      originalRows: knitting.length
      originalStitches: 200
    result

  emptyRow = (row) ->
    stitches = (s for s in row when s.type != "no" and s.type != "empty")
    stitches.length == 0

  noOverlap = (row1, row2) ->
    zipped = _.zip(row1, row2)
    _.every(zipped, (a) ->
      a[0].type == "no" or a[1].type == "no" or (a[0].type == "empty" and a[1].type == "empty")
    )

  mergeRows = (row1, row2) ->
    zipped = _.zip(row1, row2)
    _.map(zipped, (r) ->
      if r[0].type == "no" then r[1]
      else r[0]
    )

  drawStitch = (color, ctx) ->
    darker = changeLuminance(color, -0.1)
    brighter = changeLuminance(color, 0.1)

    ctx.save()
    ctx.translate(7.7, -2)
    ctx.rotate(0.38)
    ctx.scale(2 / 10, 9 / 10)
    ctx.fillStyle = brighter
    ctx.beginPath()
    ctx.arc(5, 5, 10.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = color
    ctx.beginPath()
    ctx.arc(5, 5, 5.9, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = darker
    ctx.beginPath()
    ctx.arc(5, 5, 3.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()

    ctx.save()
    ctx.translate(-0.8, 1.1)
    ctx.rotate(-0.38)
    ctx.scale(2 / 10, 9 / 10)
    ctx.fillStyle = brighter
    ctx.beginPath()
    ctx.arc(5, 5, 10.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = color
    ctx.beginPath()
    ctx.arc(5, 5, 5.9, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = darker
    ctx.beginPath()
    ctx.arc(5, 5, 3.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


  drawPurlStitch = (color, ctx) ->
    bg = changeLuminance(color, -0.3)
    darker = changeLuminance(color, -0.1)
    brighter = changeLuminance(color, 0.1)

    ctx.save()
    ctx.translate(-2, 0)
    ctx.fillStyle = bg
    ctx.fillRect(0, 0, 10, 10)
    ctx.restore()

    ctx.save()
    g = ctx.createLinearGradient(0, 0, 10, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(5, 5, 5, Math.PI, Math.PI * 2, false)
    ctx.fill()
    ctx.closePath()

    g = ctx.createLinearGradient(-5, 0, 5, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(0, 5, 5, 0, Math.PI, false)
    ctx.fill()
    ctx.closePath()

    ctx.restore()


  drawCastOnStitch = (color, ctx) ->
    darker = changeLuminance(color, -0.1)
    brighter = changeLuminance(color, 0.1)
    ctx.save()
    g = ctx.createLinearGradient(-5, 0, 5, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(0, -2, 5, 0, Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


  drawCastOffStitch = (color, ctx) ->
    darker = changeLuminance(color, -0.1)
    brighter = changeLuminance(color, 0.1)
    ctx.save()
    g = ctx.createLinearGradient(0, 0, 10, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(5, 8, 5.5, Math.PI, Math.PI * 2, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


  changeLuminance = (color, luminance) ->
    hex = color.replace(/[^0-9a-f]/gi, '')
    if (hex.length < 6)
      hex = hex[0] + hex[0] + hex[1] + hex[1] + hex[2] + hex[2]
    luminance = luminance || 0;
    rgb = "#"
    for i in [0..2]
      c = parseInt(hex.substr(i * 2, 2), 16)
      c = Math.min(255, Math.max(0, c + c * luminance))
      comp = Math.round(c).toString(16)
      rgb += ("00" + comp).substr(comp.length)
    rgb


  module = {
#   Size of a knitted in stitches/rows
    sizeOf: (knitted) ->
      knitted = reduceToKnitted(knitted)
      result =
        rows: knitted.rows.length
        stitches: knitted.stitches
        originalRows: knitted.originalRows
        originalStitches: knitted.originalStitches
        stitchOffset: knitted.stitchOffset
      result


#   Draws the stitches onto the provided canvas context
    drawStitches: (knitted, ctx, stitchAspectRatio = 0.8, stitchWidth = 20) ->
      knitted = reduceToKnitted(knitted)
      stitchHeight = stitchWidth * stitchAspectRatio
      relevantRows = knitted.rows
      ctx.translate(0, (relevantRows.length - 1) * stitchHeight)
      for row in relevantRows
        ctx.save()
        for stitch in row.data
          switch stitch.type
            when "plain"
              ctx.save()
              ctx.scale(stitchWidth / 10, stitchHeight / 10)
              drawStitch(stitch.yarns[0].color, ctx)
              ctx.restore()
            when "purl"
              ctx.save()
              ctx.scale(stitchWidth / 10, stitchHeight / 10)
              drawPurlStitch(stitch.yarns[0].color, ctx)
              ctx.restore()
            when "castOn"
              ctx.save()
              ctx.scale(stitchWidth / 10, stitchHeight / 10)
              drawCastOnStitch(stitch.yarns[0].color, ctx)
              ctx.restore()
            when "castOff"
              ctx.save()
              ctx.scale(stitchWidth / 10, stitchHeight / 10)
              drawCastOffStitch(stitch.yarns[0].color, ctx)
              ctx.restore()
            when "no"
              ctx.lineWidth = stitchWidth / 10
              ctx.strokeStyle = "#000000"
              ctx.beginPath()
              ctx.moveTo(0, 0)
              ctx.lineTo(stitchWidth, stitchHeight)
              ctx.moveTo(stitchWidth, 0)
              ctx.lineTo(0, stitchHeight)
              ctx.stroke()
          ctx.translate(stitchWidth, 0)
        ctx.restore()
        ctx.translate(0, -stitchHeight)
      result =
        rows: knitted.rows.length
        stitches: knitted.stitches
        originalRowToRow: (index) -> knitted.originalRowToRow(index)
        stitchOffset: knitted.stitchOffset
        originalRowToCoordinates: (index) ->
          (@rows - @originalRowToRow(index) - 1) * stitchHeight
        originalStitchToCoordinates: (index) ->
          if index <= @stitchOffset then 0
          else if index - @stitchOffset >= @stitches then @stitches * stitchWidth
          else (index - @stitchOffset) * stitchWidth
      result
  }
  module
)