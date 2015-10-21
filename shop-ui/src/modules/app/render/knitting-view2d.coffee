render = require('./stitch-render')
EffectiveKnittingArea = require('./knitting-areas').EffectiveKnittingArea
MarkedArea = require('./knitting-areas').MarkedArea

module.exports = (m) ->
  m.directive('knittingView2d', ($window) ->
    replace: true
    scope:
      knitting: '='

    template: """
      <canvas style="width: 100%; height: height: 100%"></canvas>
      """

    link: (scope, elem) ->
      canvas = elem[0]
      ctx = canvas.getContext("2d")
      canvas.width = 1
      canvas.height = 1
      stitchSize = 10

      draw = () ->
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        if scope.knitting
          t0 = performance.now()
          data = render.parseJson(scope.knitting, stitchSize)
          ctx.save()
          k = new EffectiveKnittingArea(data.mainBed)

          oversampling = 2
          canvas.width = $(canvas).width() * oversampling
          canvas.height = $(canvas).height() * oversampling
          drawingWidth = k.width() * stitchSize
          drawingHeight = k.height() * stitchSize
          scale = Math.min(canvas.width / drawingWidth, canvas.height / drawingHeight)
          ctx.scale(scale, scale)
          ctx.translate((canvas.width / scale - drawingWidth) / 2,
            (canvas.height / scale - drawingHeight) / 2)

          render.renderStitches(ctx, stitchSize)(k)
          ctx.restore()
          t1 = performance.now()
          console.log("Rendering took #{t1 - t0} ms")

      scope.$watch('knitting', draw)
      $($window).resize(draw)
  )
