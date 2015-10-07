render = require('./stitch-render')

module.exports = (m) ->
  m.directive('knittingView2d', ->
    replace: true
    scope:
      knitting: '='

    template: """
      <canvas></canvas>
      """

    link: (scope, elem) ->
      canvas = elem[0]
      ctx = canvas.getContext("2d")
      canvas.width = 10*200
      canvas.height = 10*350

      scope.$watch('knitting', (knitting) ->
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        if knitting
          data = render.parseJson(knitting, 10)
          ctx.save()
          for row in data.mainBed
            ctx.save()
            for stitch in row
              stitch.render(ctx)
              ctx.translate(10, 0)
            ctx.restore()
            ctx.translate(0, 10)
          ctx.restore()
      )
  )
