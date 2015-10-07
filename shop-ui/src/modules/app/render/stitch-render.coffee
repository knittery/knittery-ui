_ = require('underscore')

changeLuminance = (color, luminance) ->
  hex = color.replace(/[^0-9a-f]/gi, '')
  if (hex.length < 6)
    hex = hex[0] + hex[0] + hex[1] + hex[1] + hex[2] + hex[2]
  luminance = luminance || 0;
  rgb = "#"
  for i in [0..2]
    c = parseInt(hex.substr(i * 2, 2), 16)
    c = Math.min(255, Math.max(0, c + c * luminance))
    comp = Math.round(c).toString(16)
    rgb += ("00" + comp).substr(comp.length)
  rgb


class Stitch
  constructor: (@size) ->
  render: (ctx) ->

class PlainStitch extends Stitch
  constructor: (@color, @size) ->
  render: (ctx) ->
    darker = changeLuminance(@color, -0.1)
    brighter = changeLuminance(@color, 0.1)
    ctx.save()
    ctx.translate(7.7, -2)
    ctx.rotate(0.38)
    ctx.scale(2 / 10, 9 / 10)
    ctx.fillStyle = brighter
    ctx.beginPath()
    ctx.arc(5, 5, 10.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = @color
    ctx.beginPath()
    ctx.arc(5, 5, 5.9, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = darker
    ctx.beginPath()
    ctx.arc(5, 5, 3.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()
    ctx.save()
    ctx.translate(-0.8, 1.1)
    ctx.rotate(-0.38)
    ctx.scale(2 / 10, 9 / 10)
    ctx.fillStyle = brighter
    ctx.beginPath()
    ctx.arc(5, 5, 10.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = @color
    ctx.beginPath()
    ctx.arc(5, 5, 5.9, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.fillStyle = darker
    ctx.beginPath()
    ctx.arc(5, 5, 3.5, 0, 2 * Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


class PurlStitch extends Stitch
  constructor: (@color, @size) ->
  render: (ctx) ->
    bg = changeLuminance(@color, -0.3)
    darker = changeLuminance(@color, -0.1)
    brighter = changeLuminance(@color, 0.1)
    ctx.save()
    ctx.translate(-2, 0)
    ctx.fillStyle = bg
    ctx.fillRect(0, 0, 10, 10)
    ctx.restore()
    ctx.save()
    g = ctx.createLinearGradient(0, 0, 10, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(5, 5, 5, Math.PI, Math.PI * 2, false)
    ctx.fill()
    ctx.closePath()
    g = ctx.createLinearGradient(-5, 0, 5, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(0, 5, 5, 0, Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


class CastOnStitch extends Stitch
  constructor: (@color, @size) ->
  render: (ctx) ->
    darker = changeLuminance(@color, -0.1)
    brighter = changeLuminance(@color, 0.1)
    ctx.save()
    g = ctx.createLinearGradient(-5, 0, 5, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(0, -2, 5, 0, Math.PI, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


class CastOffStitch extends Stitch
  constructor: (@color, @size) ->
  render: (ctx) ->
    darker = changeLuminance(@color, -0.1)
    brighter = changeLuminance(@color, 0.1)
    ctx.save()
    g = ctx.createLinearGradient(0, 0, 10, 0)
    g.addColorStop(0, darker)
    g.addColorStop(0.5, brighter)
    g.addColorStop(1, darker)
    ctx.fillStyle = g
    ctx.beginPath()
    ctx.arc(5, 8, 5.5, Math.PI, Math.PI * 2, false)
    ctx.fill()
    ctx.closePath()
    ctx.restore()


parseYarns = (yarns) ->
  result = {}
  result[yarn.name] = {color: yarn.color} for yarn in yarns
  result

stitchFromJson = (json, yarns, size) ->
  switch json.type
    when 'plain'
      yarn = yarns[json.yarns[0]]
      new PlainStitch(yarn.color, size)
    when 'purl'
      yarn = yarns[json.yarns[0]]
      new PurlStitch(yarn.color, size)
    when 'castOn'
      yarn = yarns[json.yarns[0]]
      new CastOnStitch(yarn.color, size)
    when 'castOff'
      yarn = yarns[json.yarns[0]]
      new CastOffStitch(yarn.color, size)
    else
      new Stitch(size)

matrixMap = (matrix, fun) ->
  _.map(matrix, (e) -> e.map(fun))

parseJson = (json, size) ->
  yarns = parseYarns(json.yarns)
  stitchFun = (stitch) -> stitchFromJson(stitch, yarns, size)
  yarns: yarns
  mainBed: matrixMap(json.mainBed, stitchFun)
  doubleBed: matrixMap(json.doubleBed, stitchFun)


module.exports =
  parseJson: parseJson
