###
  Needle visualizations.

  Needles: Needle bar with a each needle drawn at the position it is looks on the knitting machine.
    Needle positions are taken from data-needles="ABBBDE" of the element.
  Usage: $(".needles").needles(200, "needles")

  Needle Labels: Labels for a needle bar, that shows needle numbers
  Usage: $(".labels).needleLabels(200)
###
define(["jquery"], ($) ->
  $.fn.extend({
    needleLabels: (needleCount) -> this.each(->
      root = $(this)
      canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
      canvasJ.appendTo(root)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")

      width = canvas.width = canvasJ.width() * 2
      height = canvas.height = canvasJ.height() * 2
      factor = 0.85
      baseline = 1
      needleWidth = width / needleCount
      position = (index) -> needleWidth * index + (needleWidth / 2)
      draw = (index) ->
        nr = if (index < 100) then -100 + index else index - 99
        ctx.fillText(nr, position(index), baseline, needleWidth)

      ctx.font = Math.floor(height * factor) + "px Arial"
      ctx.fillStyle = "#000000"
      ctx.textAlign = "center"
      ctx.textBaseline = "hanging"
      for n in [0..needleCount] when (n <= 100 && n % 10 == 0) or (n >= 100 && (n + 1) % 10 == 0)
        draw(n)
    )

    needles: (needleCount, dataName = "needles", downwards = true) -> this.each(->
      root = $(this)
      canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
      canvasJ.appendTo(root)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")

      needleWidth = 5
      width = canvas.width = needleCount * needleWidth
      height = canvas.height = 10

      background = "#FFFFFF"

      drawNeedle = (value) ->
        if (value.toUpperCase() == value) then ctx.fillStyle = "#222222"
        else ctx.fillStyle = "#CC2222"
        h = switch value.toUpperCase()
          when "A" then 1
          when "B" then 4
          when "D" then 7
          when "E" then 10
        if downwards then ctx.fillRect(1, 0, needleWidth - 1, h)
        else ctx.fillRect(1, 10 - h, needleWidth - 1, 10)

      draw = () ->
        values = root.data(dataName)
        if not typeIsArray(values)
          av = []
          av.push(v) for v in values
          values = av

        ctx.save()
        ctx.fillStyle = background
        ctx.fillRect(0, 0, width, height)
        for value in values
          drawNeedle(value)
          ctx.translate(needleWidth, 0)
        ctx.restore()

      root.bind("changed", () -> draw())
      root.bind("updated", () -> draw())
      root.bind(dataName + ":data", () -> draw())
      draw()
      root
    )
  })

  typeIsArray = Array.isArray || (value) -> return {}.toString.call(value) is '[object Array]'
  $
)