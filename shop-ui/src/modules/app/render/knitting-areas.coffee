_ = require('underscore')

MAX_INDEX = 9999

## Hides out areas that only consist of empty stitches (or stitches marked as hidden)
class EffectiveKnittingArea
  constructor: (@raw, hiddenMarks...) ->
    hiddenMarks ?= []
    emptyStitch = (stitch) -> stitch.empty or _.some(hiddenMarks, (mark) -> stitch.hasMark(mark))
    nonEmptyStitch = _.negate(emptyStitch)
    nonEmptyRow = (row) -> _.some(row, nonEmptyStitch)
    @firstColumn = _.min(_.map(@raw, (e) ->
      index = _.findIndex(e, nonEmptyStitch)
      if index == -1 then MAX_INDEX else index))
    @lastColumn = _.max(_.map(@raw, (e) -> _.findLastIndex(e, nonEmptyStitch)))
    cutRow = (row) => row.slice(@firstColumn, @lastColumn + 1)
    @firstRow = _.findIndex(@raw, nonEmptyRow)
    @lastRow = _.findLastIndex(@raw, nonEmptyRow)
    @rows = _.map(@raw.slice(@firstRow, @lastRow + 1), cutRow)
  width: () => @lastColumn - @firstColumn + 1
  height: () => @rows.length


## Takes a subset of rows of the knitting based on stitch marks
class MarkedRowArea
  constructor: (raw, startMark, endMark, includeStart = true, includeEnd = false) ->
    startRow =
      if startMark? then _.findIndex(raw, (r) -> _.some(r, (s) -> s.hasMark(startMark)))
      else 0
    endRow =
      if endMark? then _.findLastIndex(raw, (r) -> _.some(r, (s) -> s.hasMark(endMark)))
      else raw.length - 1
    if startRow == -1 then throw new Error("Could not find mark #{startMark}")
    if endRow == -1 then throw new Error("Could not find mark #{endMark}")
    if not includeStart then startRow = Math.min(startRow + 1, raw.length - 1)
    if includeEnd then endRow = Math.min(endRow + 1, raw.length - 1)
    @rows = raw.slice(startRow, endRow)
  width: () => @rows[0].length
  height: () => @rows.length

## Takes a subset of columns of the knitting based on stitch marks
class MarkedColumnArea
  constructor: (raw, startMark, endMark, includeStart = false, includeEnd = false) ->
    colCount = raw[0].length
    start =
      if startMark? then _.min(_.map(raw, (r) ->
        idx = _.findIndex(r, (s) -> s.hasMark(startMark))
        if idx == -1 then MAX_INDEX else idx))
      else 0
    end =
      if endMark? then _.max(_.map(raw, (r) -> _.findLastIndex(r, (s) -> s.hasMark(endMark))))
      else colCount - 1
    if start == -1 then throw new Error("Could not find mark #{startMark}")
    if end == -1 then throw new Error("Could not find mark #{endMark}")
    if not includeStart then start = Math.min(start + 1, colCount - 1)
    if includeEnd then end = Math.min(end + 1, colCount - 1)
    @rows = _.map(raw, (r) -> r.slice(start, end))
  width: () => @rows[0].length
  height: () => @rows.length

module.exports =
  EffectiveKnittingArea: EffectiveKnittingArea
  MarkedRowArea: MarkedRowArea
  MarkedColumnArea: MarkedColumnArea
