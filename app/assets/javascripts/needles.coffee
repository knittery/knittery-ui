jQuery.fn.extend({
  ###
  Shows a needle board using 2d-canvas.
  ###
  needles: (needleCount) ->
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
      ctx.fillStyle = "#222222"
      switch value
        when "A" then ctx.fillRect(1, 0, needleWidth-1, 1)
        when "B" then ctx.fillRect(1, 0, needleWidth-1, 4)
        when "D" then ctx.fillRect(1, 0, needleWidth-1, 7)
        when "E" then ctx.fillRect(1, 0, needleWidth-1, 10)
          
    draw = () ->
      values = root.data("needles")
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

    root.bind("changeData", (e, key, value) ->
      if key == "needles" then draw()
    )
    draw()  	
})

typeIsArray = Array.isArray || ( value ) -> return {}.toString.call( value ) is '[object Array]'