### Shows a two dimensional view of the knitted output using 2d-canvas. ###
jQuery.fn.extend({
  knitted2d: (dataName = "knitted", fit = "knitted") -> this.each(->
    root = $(this)
    canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
    canvasJ.appendTo(root)
    canvas = canvasJ.get(0)
    ctx = canvas.getContext("2d")
    oversampling = 2

    draw = () ->
      output = if root.data(dataName)? then root.data(dataName) else []
      canvas.width = canvasJ.width() * oversampling
      canvas.height = canvasJ.height() * oversampling
      switch fit
        when "full-width"
          image = renderKnitted(output)
          ctx.drawImage(image, 0, 0, canvas.width, canvas.width * image.height / image.width)
        when "knitted"
          image = renderKnitted(reduceToKnitted(output))
          [w, h] = scaleToFit(image, {width: canvas.width * 0.95, height: canvas.height * 0.95})
          ctx.translate((canvas.width - w) / 2, (canvas.height - h) / 2)
          ctx.drawImage(image, 0, 0, w, h)

    root.bind("changed", () -> draw())
    root.bind("updated", () -> draw())
    root.bind(dataName + ":data", () -> draw())
    draw()
    root
  )
})


renderKnitted = (knitted) ->
  canvas = document.createElement("canvas")
  ctx = canvas.getContext("2d")

  relevantRows = (r for r in knitted when not emptyRow(r))
  rows = relevantRows.length
  columns = if rows > 0 then relevantRows[0].length else 200

  stitchWidth = 20
  stitchAspectRatio = 0.8
  stitchHeight = stitchWidth * stitchAspectRatio
  canvas.width = columns * stitchWidth
  canvas.height = rows * stitchHeight

  ctx.translate(0, (rows - 1) * stitchHeight)
  for row in relevantRows
    i = i + 1
    if i >= rows then break
    ctx.save()
    for stitch in row
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
          ctx.fillStyle = stitch.yarns[0].color
          ctx.fillRect(0, 0, stitchWidth, stitchHeight / 2)
          ctx.fillStyle = "#000000"
          ctx.fillRect(0, stitchHeight / 2, stitchWidth, stitchHeight / 2)
        when "castOff"
          ctx.fillStyle = "#000000"
          ctx.fillRect(0, 0, stitchWidth, stitchHeight / 2)
          ctx.fillStyle = stitch.yarns[0].color
          ctx.fillRect(0, stitchHeight / 2, stitchWidth, stitchHeight / 2)
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

  canvas

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


reduceToKnitted = (knitting) ->
  first = 200
  last = 0
  rows = for row in knitting when !emptyRow(row)
    for stitch, i in row
      if not (stitch.type == "no" or stitch.type == "empty")
        first = Math.min(first, i)
        last = Math.max(last, i)
    row
  row.slice(first, last) for row in rows

emptyRow = (row) ->
  stitches = (s for s in row when s.type != "no" and s.type != "empty")
  stitches.length == 0

scaleToFit = (toScale, container) ->
  scale = Math.min(container.width / toScale.width, container.height / toScale.height)
  [toScale.width * scale, toScale.height * scale]