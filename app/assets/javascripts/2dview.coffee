### Shows a two dimensional view of the knitted output using 2d-canvas. ###
jQuery.fn.extend({
  knitted2d: (dataName = "knitted", mode = "topLeft") -> this.each(->
    root = $(this)
    canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
    canvasJ.appendTo(root)
    canvas = canvasJ.get(0)
    ctx = canvas.getContext("2d")
    oversampling = 2

    draw = () ->
      output = if root.data(dataName)? then root.data(dataName) else []
      image = renderKnitted(output)
      canvas.width = canvasJ.width() * oversampling
      canvas.height = canvasJ.height() * oversampling
      switch mode
        when "topLeft"
          ctx.drawImage(image, 0, 0)
        when "scale"
          [w, h] = scaleToFit(canvas, {width: canvas.width * 0.95, height: canvas.height * 0.95})
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

  columns = 200
  relevantRows = (r for r in knitted when not emptyRow(r))
  rows = relevantRows.length

  stitchWidth = 10
  stitchAspectRatio = 0.8
  stitchHeight = stitchWidth * stitchAspectRatio
  canvas.width = columns * stitchWidth
  canvas.height = rows * stitchHeight

  for row in relevantRows.reverse()
    i = i + 1
    if i >= rows then break
    for stitch in row
      switch stitch.type
        when "plain"
          ctx.fillStyle = stitch.yarns[0].color
          ctx.fillRect(0, 0, stitchWidth, stitchHeight)
        when "purl"
          ctx.fillStyle = stitch.yarns[0].color
          ctx.fillRect(0, 0, stitchWidth, stitchHeight)
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
    ctx.translate(-columns * stitchWidth, stitchHeight)

  canvas


emptyRow = (row) ->
  stitches = (s for s in row when s.type != "no" and s.type != "empty")
  stitches.length == 0


scaleToFit = (toScale, container) ->
  scale = Math.min(container.width / toScale.width, container.height / toScale.height)
  [toScale.width * scale, toScale.height * scale]