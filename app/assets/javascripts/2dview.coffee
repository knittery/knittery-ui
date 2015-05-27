###
  2D representation of a knitting. Use data-knitted="<json>" to set the values to display.
  Attributes supported:
    - from-row: first row to display
    - to-row: last row to display
    - fit: either
      - full-width: the whole 200 needles are displayed
      - knitted: the whole knitting is displayed (scaled)

  Usage: $(".knitting").knitted2d()
###
define(["jquery", "underscorejs", "canvas-manipulation", "2drender"], ($, _, CanvasManipulation, stitchRenderer) ->
  knittingRenderer = (knitted) ->
    size = stitchRenderer.sizeOf(knitted)
    canvas = document.createElement("canvas")
    ctx = canvas.getContext("2d")
    aspectRatio = 0.8
    stitchWidth = 20
    stitchHeight = stitchWidth * aspectRatio
    canvas.width = size.stitches * stitchWidth
    canvas.height = size.rows * stitchHeight

    renderMetadata = stitchRenderer.drawStitches(knitted, ctx, aspectRatio, stitchWidth)

    result =
      draw: (toCtx, fromRow = 0, toRow = 9999999) ->
#       draws at 0, 0 with a row height of 1, use toCtx.scale/translate before.
        fromRenderedRow = renderMetadata.originalRowToRow(fromRow)
        toRenderedRow = renderMetadata.originalRowToRow(toRow)
        count = toRenderedRow - fromRenderedRow + 1
        toCtx.drawImage(canvas, 0, (renderMetadata.rows - toRenderedRow - 1) * stitchHeight, canvas.width, count * stitchHeight
          @xOffset, 0, @width, count)
      fullWidth: size.originalStitches / aspectRatio
      width: size.stitches / aspectRatio
      xOffset: size.stitchOffset / aspectRatio
      height: (fromRow = 0, toRow = 9999999) ->
        fromRenderedRow = renderMetadata.originalRowToRow(fromRow)
        toRenderedRow = renderMetadata.originalRowToRow(toRow)
        toRenderedRow - fromRenderedRow + 1
    result


  $.fn.extend({
    knitted2d: (allowZoom = false, dataName = "knitted") -> this.each(->
      root = $(this)
      canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
      canvasJ.appendTo(root)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")
      oversampling = 2

      createImage = () ->
        output = if root.data(dataName)? then root.data(dataName) else []
        renderer = knittingRenderer(output)
        console.debug("created 2d knitting with size #{renderer.width} x #{renderer.height()})")
        control = new CanvasManipulation(canvas, () ->)

        updateImage = () ->
          fromRow = root.attr("from-row")
          fromRow = if (fromRow?) then fromRow else 0
          toRow = root.attr("to-row")
          toRow = if (toRow?) then toRow else output.length

          ctx.setTransform(1, 0, 0, 1, 0, 0)
          ctx.clearRect(0, 0, canvas.width, canvas.height)
          control.applyTransformation(ctx)
          renderer.draw(ctx, fromRow, toRow)

        updateSize = () ->
          canvas.width = canvasJ.width() * oversampling
          canvas.height = canvasJ.height() * oversampling

          control.reset()
          fit = root.attr("fit")
          fit = if (fit?) then fit else "full-width"
          switch fit
            when "full-width"
              factor = canvas.width / renderer.fullWidth
              console.debug("scale of knitting is #{factor}")
              control.scale(factor, factor)
            when "knitted"
              fromRow = root.attr("from-row")
              fromRow = if (fromRow?) then fromRow else 0
              toRow = root.attr("to-row")
              toRow = if (toRow?) then toRow else output.length
              rh = renderer.height(fromRow, toRow)
              factor = Math.min(canvas.width / renderer.width, canvas.height / rh)
              xOffset = (canvas.width / factor - renderer.width) / 2 - renderer.xOffset
              yOffset = (canvas.height / factor - rh) / 2
              control.scale(factor, factor)
              control.move(xOffset, yOffset)
              console.debug("scale of knitting is #{factor} with offset (#{xOffset}, #{yOffset})")
          updateImage()

        control.repaint = updateImage
        if allowZoom then control.registerHandlers()

        root.bind("from-row:attr", updateImage)
        root.bind("to-row:attr", updateImage)
        root.bind("fit:attr", updateImage)
        root.resize(updateImage)
        updateSize()

      root.bind(dataName + ":data", () -> createImage())
      createImage()
      root
    )
  })
)