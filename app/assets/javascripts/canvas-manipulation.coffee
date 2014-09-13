define(["jquery"], ($) ->
  svg = document.createElementNS("http://www.w3.org/2000/svg", "svg")

  userSelect = (enable) ->
    value = if enable then "auto" else "none"
    document.body.style.mozUserSelect = value
    document.body.style.webkitUserSelect = value
    document.body.style.userSelect = value

  transformedPoint = (p, matrix) ->
    point = svg.createSVGPoint()
    point.x = p.x
    point.y = p.y
    point.matrixTransform(matrix.inverse())

  class TransformedPoint
    constructor: (element, matrixProvider) ->
      xFactor = (element.width / $(element).width())
      yFactor = (element.height / $(element).height())
      @update = (event) ->
        eventPos =
          x: event.clientX - $(element).offset().left
          y: event.clientY - $(element).offset().top
        eventPos.x = eventPos.x * xFactor
        eventPos.y = eventPos.y * yFactor
        p = transformedPoint(eventPos, matrixProvider.matrix)
        @x = p.x
        @y = p.y
        p
    x: 0
    y: 0


  class CanvasManipulation
    constructor: (@canvas, @repaint) ->

    registerHandlers: () ->
      @addDragHandler(@canvas)
      @addZoomHandler(@canvas)

    matrix: svg.createSVGMatrix()

    reset: () -> @matrix = svg.createSVGMatrix()
    move: (x, y) -> @matrix = @matrix.translate(x, y)
    rotate: (radians) -> @matrix = @matrix.rotate(radians * 180 / Math.PI)
    scale: (sx, sy) -> @matrix = @matrix.scaleNonUniform(sx, sy)

    applyTransformation: (ctx) ->
      ctx.transform(@matrix.a, @matrix.b, @matrix.c, @matrix.d, @matrix.e, @matrix.f)

    addDragHandler: (to) ->
      lastPos = new TransformedPoint(@canvas, this)
      @lastPos = lastPos
      self = this
      $(to).mousedown((event) ->
        userSelect(false)
        self.draggingStartedAt = lastPos.update(event)
      )
      $(document).mousemove((event) ->
        lastPos.update(event)
        if self.draggingStartedAt?
          self.move(lastPos.x - self.draggingStartedAt.x,
              lastPos.y - self.draggingStartedAt.y)
          self.repaint()
      )
      $(document).mouseup(() ->
        self.draggingStartedAt = null
        userSelect(true)
      )

    zoomFactorPerClick: 1.1

    addZoomHandler: (to) ->
      self = this
      zoom = (clicks) ->
        p = self.lastPos
        factor = Math.pow(self.zoomFactorPerClick, clicks)
        self.move(p.x, p.y)
        self.scale(factor, factor)
        self.move(-p.x, -p.y)
        self.repaint()
      handleScroll = (event) ->
        delta =
          if event.wheelDelta? then event.wheelDelta / 40
          else if event.detail? then -event.detail
          else 0
        if delta? then zoom(delta)
        event.preventDefault()
        false
      to.addEventListener("DOMMouseScroll", handleScroll, false);
      to.addEventListener("mousewheel", handleScroll, false);


  CanvasManipulation
)