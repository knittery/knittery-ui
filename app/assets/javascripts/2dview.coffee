jQuery.fn.extend({
  ### Shows a two dimensional view of the knitted output using 2d-canvas. ###
  knitted2d: (dataName = "knitted", maxRows = 100, stitchWidth = 10, stitchHeight = 10) -> this.each(->
    root = $(this)
    canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
    canvasJ.appendTo(root)
    canvas = canvasJ.get(0)
    ctx = canvas.getContext("2d")

    w = canvas.width = stitchWidth * 200
    h = canvas.height = stitchHeight * maxRows

    draw = () ->
      output = root.data(dataName)
      ctx.clearRect(0, 0, w, h)
      ctx.save()
      i = 0
      if output?
        relevantRows = (r for r in output when not emptyRow(r))
        #ctx.translate(0, stitchHeight * Math.min(relevantRows.length, maxRows))
        for row in relevantRows.reverse()
          i = i + 1
          if i >= maxRows then break
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
          ctx.translate(-w, stitchHeight)
        ctx.translate(0, -h)
        ctx.restore()

    root.bind("changed", () -> draw())
    root.bind("updated", () -> draw())
    root.bind(dataName + ":data", () -> draw())
    draw()
    root
  )
})

emptyRow = (row) ->
  stitches = (s for s in row when s.type != "no" and s.type != "empty")
  stitches.length == 0