_ = require('underscore')

### A knitting (or a cutted part of it), used to assemble the product from. ###
class Cutpiece
  constructor: (@rows) ->
  width: () => if @rows.length > 0 then @rows[0].length else undefined
  height: () => @rows.length
  mirrorRows: () => new Cutpiece(@rows.reverse())

### Part of a Cutpiece. ###
class SubCutpiece extends Cutpiece
  ###
    @constructor
    @param {KnittingArea} base - Base of the view
    @param {Object} views - Format: {rows: {from:1, until:2}, columns: {from:0,until:199}} where From is inclusive and
      until is exclusive (same a Array.slice).
  ###
  constructor: (@base, @view) ->
    @view.rows ?= {from: 0, until: @base.height()}
    @view.columns ?= {from: 0, until: @base.width()}
    cutRow = (row) => row.slice(@view.columns.from, @view.columns.until)
    @rows = _.map(@base.rows.slice(@view.rows.from, @view.rows.until), cutRow)
  unwrap: => @base

class HorizontalComposedCutpiece extends Cutpiece
  constructor: (@left, @right, fillWith) ->
    pairs = _.zip(@left.rows, @right.rows)
    @rows = for [l,r] in pairs
      l ?= (fillWith for i in [1..@left.width()])
      r ?= (fillWith for i in [1..@right.width()])
      l.concat(r)


MAX_INDEX = Math.pow(2, 53) - 1 #Number.MAX_SAFE_INTEGER

### Create a new cutpiece
    @param {object[][]} rows - Stitch[Row][Column] ###
cutpiece = (rows) -> new Cutpiece(rows)

### Removes leading/tailing rows that only consist of empty stitches (or stitches marked as hidden). ###
effectiveKnittedRows = (base, hiddenMarks...) ->
  hiddenMarks ?= []
  emptyStitch = (stitch) -> stitch.empty or _.some(hiddenMarks, (mark) -> stitch.hasMark(mark))
  nonEmptyRow = (row) -> _.some(row, _.negate(emptyStitch))
  new SubCutpiece(base, rows:
    from: _.findIndex(base.rows, nonEmptyRow)
    until: _.findLastIndex(base.rows, nonEmptyRow) + 1)

### Removes leading/tailing columns that only consist of empty stitches (or stitches marked as hidden). ###
effectiveKnittedColumns = (base, hiddenMarks...) ->
  hiddenMarks ?= []
  emptyStitch = (stitch) -> stitch.empty or _.some(hiddenMarks, (mark) -> stitch.hasMark(mark))
  nonEmptyStitch = _.negate(emptyStitch)
  firstColumn = _.min(_.map(base.rows, (e) ->
    index = _.findIndex(e, nonEmptyStitch)
    if index == -1 then MAX_INDEX else index))
  lastColumn = _.max(_.map(base.rows, (e) -> _.findLastIndex(e, nonEmptyStitch)))
  new SubCutpiece(base, columns:
    from: firstColumn
    until: lastColumn + 1)

### Remove leading/trailing columns and rows that only consist of empty stitches (or stitches marked as hidden). ###
effectiveKnitted = (base, hiddenMarks...) ->
  effectiveKnittedColumns(effectiveKnittedRows(base, hiddenMarks), hiddenMarks)


## Takes a subset of rows of the knitting based on stitch marks
betweenMarkedRows = (base, startMark, endMark, includeStart = true, includeEnd = false) ->
  start =
    if startMark? then _.findIndex(base.rows, (r) -> _.some(r, (s) -> s.hasMark(startMark)))
    else 0
  end =
    if endMark? then _.findLastIndex(base.rows, (r) -> _.some(r, (s) -> s.hasMark(endMark)))
    else base.height() - 1
  if start == -1 then throw new Error("Could not find mark #{startMark}")
  if end == -1 then throw new Error("Could not find mark #{endMark}")
  if not includeStart then start = Math.min(start + 1, raw.length - 1)
  if includeEnd then end = Math.min(end + 1, raw.length - 1)
  new SubCutpiece(base, rows:
    from: start
    until: end)

## Takes a subset of columns of the knitting based on stitch marks
betweenMarkedColumns = (base, startMark, endMark, includeStart = false, includeEnd = false) ->
  raw = ""
  start =
    if startMark? then _.min(_.map(base.rows, (r) ->
      idx = _.findIndex(r, (s) -> s.hasMark(startMark))
      if idx == -1 then MAX_INDEX else idx))
    else 0
  end =
    if endMark? then _.max(_.map(base.rows, (r) -> _.findLastIndex(r, (s) -> s.hasMark(endMark))))
    else base.width() - 1
  if start == -1 then throw new Error("Could not find mark #{startMark}")
  if end == -1 then throw new Error("Could not find mark #{endMark}")
  if not includeStart then start = Math.min(start + 1, base.width() - 1)
  if includeEnd then end = Math.min(end + 1, base.width() - 1)
  new SubCutpiece(base, columns:
    from: start
    until: end)

effectiveBetweenMarkedRows = (base, startMark, endMark, includeStart = true, includeEnd = false, hiddenMarks...) ->
  effectiveKnittedColumns(betweenMarkedRows(base, startMark, endMark, includeStart, includeEnd), hiddenMarks)

composeHorizontal = (left, right, fillWith) ->
  new HorizontalComposedCutpiece(left, right, fillWith)


module.exports =
  create: cutpiece
  effectiveKnittedRows: effectiveKnittedRows
  effectiveKnittedColumns: effectiveKnittedColumns
  effectiveKnitted: effectiveKnitted
  betweenMarkedRows: betweenMarkedRows
  betweenMarkedColumns: betweenMarkedColumns
  effectiveBetweenMarkedRows: effectiveBetweenMarkedRows
  composeHorizontal: composeHorizontal
