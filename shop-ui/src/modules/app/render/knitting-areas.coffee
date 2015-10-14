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


module.exports =
  EffectiveKnitting: EffectiveKnitting
