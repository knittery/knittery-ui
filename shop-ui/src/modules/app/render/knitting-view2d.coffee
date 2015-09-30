module.exports = (m) ->
  m.directive('knittingView2d', ->
    template: """
      <canvas></canvas>
      """
    replace: true

    link: (scope, elem) ->
      canvas = elem[0]
      ctx = canvas.getContext("2d")
      canvas.width = 100
      canvas.height = 100

      ctx.fillStyle = '#ff0000'
      ctx.fillRect(1, 1, 98, 98)
  )
