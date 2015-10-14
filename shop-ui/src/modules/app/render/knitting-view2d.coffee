render = require('./stitch-render')
EffectiveKnitting = require('./knitting-areas').EffectiveKnitting

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
          k = new EffectiveKnitting(data.mainBed)

          oversampling = 2
          canvas.width = $(canvas).width() * oversampling
          canvas.height = $(canvas).height() * oversampling
          drawingWidth = k.width() * stitchSize
          drawingHeight = k.height() * stitchSize
          scale = Math.min(canvas.width / drawingWidth, canvas.height / drawingHeight)
          ctx.scale(scale, scale)
          ctx.translate((canvas.width / scale - drawingWidth) / 2,
            (canvas.height / scale - drawingHeight) / 2)

          for row in k.rows
            ctx.save()
            for stitch in row
              stitch.render(ctx)
              ctx.translate(stitchSize, 0)
            ctx.restore()
            ctx.translate(0, stitchSize)
          ctx.restore()
          t1 = performance.now()
          console.log("Rendering took #{t1 - t0} ms")

      scope.$watch('knitting', draw)
      $($window).resize(draw)
  )
