###
  Extend jQuery to draw an element as a carriage. Use data-settings="<json>" to set the data to display.

  $(".c").kCarriage()
  $(".c").doubleBedCarriage()
###
define(["jquery"], ($) ->

  drawUnpressedButtonAt = (ctx, x) ->
    ctx.beginPath()
    ctx.rect(x, 46, 10, 5)
    ctx.stroke()

  drawKCarriage = (ctx, settings) ->
    ctx.clearRect(0, 0, 130, 60)
    ctx.save()

    ctx.fillStyle = "grey"
    ctx.strokeStyle = "grey"
    ctx.lineWidth = "1"

    ctx.beginPath()
    ctx.rect(0, 0, 120, 45)
    ctx.stroke()

    ctx.beginPath()
    ctx.rect(10, 33, 15, 5)
    ctx.stroke()

    ctx.beginPath()
    ctx.arc(60, 20, 13, 0, 2 * Math.PI)
    ctx.stroke()

    ctx.font = "18px Arial"
    ctx.fillText(settings.tension.number, 55, 26)

    if(settings.tension.thirds >= 1)
      ctx.beginPath()
      ctx.arc(72, 31, 1, 0, 2 * Math.PI)
      ctx.stroke()
      if(settings.tension.thirds is 2)
        ctx.beginPath()
        ctx.arc(75, 26, 1, 0, 2 * Math.PI)
        ctx.stroke()
    ctx.font = "6px Arial"

    ctx.fillText("KCII", 106, 12)
    ctx.beginPath()
    ctx.arc(97, 15, 7, 0, 2 * Math.PI)
    ctx.stroke()
    ctx.beginPath()
    ctx.moveTo(97, 15)
    ctx.lineTo(102, 12)
    ctx.stroke()

    if settings.holdingCamLever is "N"
      ctx.fillRect(10, 33, 5, 5)
      ctx.fillText("N", 10, 28)
    else if settings.holdingCamLever is "H"
      ctx.fillRect(15, 33, 5, 5)
      ctx.fillText("H", 15, 28)
    else
      ctx.fillRect(20, 33, 5, 5)
      ctx.fillText("I", 15, 28)

    if settings.tuckLeft
      ctx.fillRect(36, 46, 10, 3)
    else
      drawUnpressedButtonAt(ctx, 36)
    if settings.tuckRight
      ctx.fillRect(48, 46, 10, 3)
    else
      drawUnpressedButtonAt(ctx, 48)

    if settings.mc
      drawUnpressedButtonAt(ctx, 60)
      ctx.fillRect(60, 46, 10, 3)
    else if settings.l
      ctx.fillRect(60, 46, 10, 3)
    else
      drawUnpressedButtonAt(ctx, 60)

    if settings.partLeft
      ctx.fillRect(72, 46, 10, 3)
    else
      drawUnpressedButtonAt(ctx, 72)
    if settings.partRight
      ctx.fillRect(84, 46, 10, 3)
    else
      drawUnpressedButtonAt(ctx, 84)

  drawDoubleBedCarriage = (ctx, settings) ->
    ctx.clearRect(0, 0, 130, 60)
    ctx.save()

    if (not settings?)
      return

    ctx.fillStyle = "grey"
    ctx.strokeStyle = "grey"
    ctx.lineWidth = "1"

    ctx.beginPath()
    ctx.rect(10, 0, 100, 45)
    ctx.stroke()

    ctx.beginPath()
    ctx.arc(60, 22, 13, 0, 2 * Math.PI)
    ctx.stroke()

    ctx.font = "18px Arial"
    ctx.fillText(settings.tension.number, 55, 28)

    if(settings.tension.thirds >= 1)
      ctx.beginPath()
      ctx.arc(72, 33, 1, 0, 2 * Math.PI)
      ctx.stroke()
      if(settings.tension.thirds is 2)
        ctx.beginPath()
        ctx.arc(75, 28, 1, 0, 2 * Math.PI)
        ctx.stroke()

    ctx.beginPath()
    ctx.arc(20, 36, 4, 0, 2 * Math.PI)
    ctx.stroke()

    ctx.beginPath()
    ctx.arc(100, 36, 4, 0, 2 * Math.PI)
    ctx.stroke()

    drawLines(ctx, 13, 25)
    drawLines(ctx, 101, 25)

    if(settings.knobLeft is "IiIi")
      ctx.beginPath()
      ctx.moveTo(20, 36)
      ctx.lineTo(18, 34)
      ctx.stroke()
    else
      ctx.beginPath()
      ctx.moveTo(20, 36)
      ctx.lineTo(22, 34)
      ctx.stroke()

    if(settings.knobRight is "IiIi")
      ctx.beginPath()
      ctx.moveTo(100, 36)
      ctx.lineTo(102, 34)
      ctx.stroke()
    else
      ctx.beginPath()
      ctx.moveTo(100, 36)
      ctx.lineTo(98, 34)
      ctx.stroke()

    ctx.font = "8px Arial"
    if (settings.slideLever is "I")
      ctx.fillText("I", 47, 43)
      ctx.beginPath()
      ctx.arc(48, 47, 2, 0, 2 * Math.PI)
      ctx.stroke()
    else if (settings.slideLever is "II")
      ctx.fillText("II", 70, 43)
      ctx.beginPath()
      ctx.arc(73, 47, 2, 0, 2 * Math.PI)
      ctx.stroke()
    else
      drawLines(ctx, 57, 38)
      ctx.beginPath()
      ctx.arc(60, 47, 2, 0, 2 * Math.PI)
      ctx.stroke()

    if (settings.needleTakebackLeft is true)
      ctx.fillText("N", 2, 17)
      ctx.beginPath()
      ctx.moveTo(1, 3)
      ctx.lineTo(8, 8)
      ctx.stroke()
    else
      ctx.fillText("H", 2, 8)
      ctx.beginPath()
      ctx.moveTo(1, 15)
      ctx.lineTo(8, 10)
      ctx.stroke()

    if (settings.needleTakebackRight is true)
      ctx.fillText("N", 113, 17)
      ctx.beginPath()
      ctx.moveTo(119, 3)
      ctx.lineTo(112, 8)
      ctx.stroke()
    else
      ctx.fillText("H", 113, 8)
      ctx.beginPath()
      ctx.moveTo(119, 15)
      ctx.lineTo(112, 10)
      ctx.stroke()

    ctx.beginPath()
    ctx.rect(18, 5, 11, 9)
    ctx.stroke()

    ctx.font = "6px Arial"

    if (settings.partLeft is false)
      ctx.fillText("N", 20, 21)
      ctx.fillRect(18, 10, 11, 3)
      ctx.stroke()
    else
      ctx.fillText("P-R", 18, 21)
      ctx.fillRect(18, 5, 11, 3)
      ctx.stroke()

    ctx.beginPath()
    ctx.rect(87, 5, 11, 9)
    ctx.stroke()

    if (settings.partRight is false)
      ctx.fillText("N", 89, 21)
      ctx.fillRect(87, 10, 11, 3)
      ctx.stroke()
    else
      ctx.fillText("P-R", 87, 21)
      ctx.fillRect(87, 5, 11, 3)
      ctx.stroke()

    ctx.beginPath()
    ctx.rect(35, 5, 8, 6)
    ctx.stroke()

    if (settings.tuckingLever is "R")
      ctx.fillText("R", 37, 18)
      ctx.fillRect(35, 8, 8, 3)
      ctx.stroke()
    else
      ctx.fillText("P", 37, 18)
      ctx.fillRect(35, 5, 8, 3)
      ctx.stroke()

  drawLines = (ctx, startX, offsetY) ->
    ctx.beginPath()
    ctx.moveTo(startX, offsetY)
    ctx.lineTo(startX, offsetY + 5)
    ctx.moveTo(startX + 2, offsetY + 2)
    ctx.lineTo(startX + 2, offsetY + 5)
    ctx.moveTo(startX + 4, offsetY)
    ctx.lineTo(startX + 4, offsetY + 5)
    ctx.moveTo(startX + 6, offsetY + 2)
    ctx.lineTo(startX + 6, offsetY + 5)
    ctx.stroke()


  $.fn.extend({
    kCarriage: () ->
      elem = $(this)
      canvasJ = $("<canvas style='width: 130; height: 60'></canvas>")
      canvasJ.appendTo(elem)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")

      draw = () ->
        settings = elem.data("settings")
        if settings? then drawKCarriage(ctx, settings)
        else ctx.clearRect(0, 0, 130, 60)
      elem.bind("settings:data", draw)
      draw()

    doubleBedCarriage: () ->
      elem = $(this)
      canvasJ = $("<canvas style='width: 130; height: 60'></canvas>")
      canvasJ.appendTo(elem)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")

      draw = () ->
        settings = elem.data("settings")
        if settings? then drawDoubleBedCarriage(ctx, settings)
        else ctx.clearRect(0, 0, 130, 60)
      elem.bind("settings:data", draw)
      draw()
  })
  $
)