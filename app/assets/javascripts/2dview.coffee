jQuery.fn.extend({
  ### Shows a two dimensional view of the knitted output using 2d-canvas. ###
  knitted2d: (dataName = "knitted", maxRows = 100, stitchWidth = 2, stitchHeight = 2) -> this.each(->
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
      if output? then for row, i in output.reverse()
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