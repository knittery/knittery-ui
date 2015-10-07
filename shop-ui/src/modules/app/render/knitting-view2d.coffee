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
      canvas.width = 100
      canvas.height = 100

      scope.$watch('knitting', (knitting) ->
        if knitting
          data = render.parseJson(knitting, 10)
          console.log(data)
      )
#      ctx.fillStyle = '#ff0000'
#      ctx.fillRect(1, 1, 98, 98)
  )
