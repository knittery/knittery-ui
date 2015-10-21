_ = require('underscore')

## Hides out areas that only consist of empty stitches
class EffectiveKnittingArea
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
  width: () => @lastColumn - @firstColumn + 1
  height: () => @rows.length


## Takes a part of the knitting based on stitch marks
class MarkedArea
  constructor: (raw, startMark, endMark) ->
    startRow =
      if startMark? then _.findIndex(raw, (r) -> _.some(r, (s) -> s.hasMark(startMark)))
      else 0
    endRow =
      if endMark? then _.findLastIndex(raw, (r) -> _.some(r, (s) -> s.hasMark(endMark)))
      else raw.length - 1
    if startRow == -1 then throw new Error("Could not find mark #{startMark}")
    if endRow == -1 then throw new Error("Could not find mark #{endMark}")
    @rows = raw.slice(startRow, endRow)
  width: () => @rows[0].length
  height: () => @rows.length


module.exports =
  EffectiveKnittingArea: EffectiveKnittingArea
  MarkedArea: MarkedArea
