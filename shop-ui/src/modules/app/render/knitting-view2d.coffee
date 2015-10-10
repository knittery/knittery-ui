render = require('./stitch-render')
_ = require('underscore')

## Hides out areas that only consist of empty stitches
class EffectiveKnitting
  constructor: (@raw) ->
    emptyStitch = (stitch) -> stitch.empty
    nonEmptyStitch = _.negate(emptyStitch)
    nonEmptyRow = (row) -> _.some(row, nonEmptyStitch)
    @firstColumn = _.min(_.map(@raw, (e) ->
      index = _.findIndex(e, nonEmptyStitch)
      if index == -1 then 9999 else index))
    @lastColumn = _.max(_.map(@raw, (e) -> _.findLastIndex(e, nonEmptyStitch)))
    cutRow = (row) => row.slice(@firstColumn, @lastColumn + 1)
    @firstRow = _.findIndex(@raw, nonEmptyRow)
    @lastRow = _.findLastIndex(@raw, nonEmptyRow)
    @rows = _.map(@raw.slice(@firstRow, @lastRow + 1), cutRow)
  width: () ->
    @lastColumn - @firstColumn + 1
  height: () ->
    @rows.length

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
      canvas.width = 1
      canvas.height = 1
      stitchSize = 10

      scope.$watch('knitting', (knitting) ->
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        if knitting
          t0 = performance.now()
          data = render.parseJson(knitting, stitchSize)
          ctx.save()
          k = new EffectiveKnitting(data.mainBed)
          canvas.width = k.width() * stitchSize
          canvas.height = k.height() * stitchSize
          for row in k.rows
            ctx.save()
            for stitch in row
              stitch.render(ctx)
              ctx.translate(stitchSize, 0)
            ctx.restore()
            ctx.translate(0, stitchSize)
          ctx.restore()
          t1 = performance.now()
          console.log("Rendering took #{t1-t0} ms")
      )
  )
