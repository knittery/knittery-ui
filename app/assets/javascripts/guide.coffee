$(() ->
  $("#next").link().disabled(guide, "step", (s) -> s? && s.last)
  $("#next").click(() ->
    jsRoutes.controllers.Guide.next().ajax()
    false
  )
  $("#prev").link().disabled(guide, "step", (s) -> s? && s.first)
  $("#prev").click(() ->
    jsRoutes.controllers.Guide.previous().ajax()
    false
  )
  $("#first").link().disabled(guide, "step", (s) -> s? && s.first)
  $("#first").click(() ->
    jsRoutes.controllers.Guide.first().ajax()
    false
  )
  $("#last").click(() ->
    jsRoutes.controllers.Guide.last().ajax()
    false
  )
  $("#last").link().disabled(guide, "step", (s) -> s? && s.last)

  $(document).keydown((e) -> switch e.which
    when 37 #left
      jsRoutes.controllers.Guide.previousInstruction().ajax()
      e.preventDefault()
    when 39 #right
      jsRoutes.controllers.Guide.nextInstruction().ajax()
      e.preventDefault()
  )

  $("#nextInstruction").link().disabled(guide, "instruction", (i) -> i? && i.last)
  $("#nextInstruction").click(() ->
    jsRoutes.controllers.Guide.nextInstruction().ajax()
    false
  )
  $("#prevInstruction").link().disabled(guide, "instruction", (i) -> i? && i.first)
  $("#prevInstruction").click(() ->
    jsRoutes.controllers.Guide.previousInstruction().ajax()
    false
  )
  $(".step-list li:not(.special-step)").each(() ->
    url = "#" + $(this).data("step")
    $(this).wrapInner("<a href=" + url + ">")
  )
  $(".step-list li a").click(() ->
    jsRoutes.controllers.Guide.jumpTo($(this).attr("href").substr(1)).ajax()
    false
  )
  $(".instruction-count").click(() ->
    target = prompt("Jump to instruction", guide.instruction.index)
    if (target?)
      if (target < 0) then target = 0
      else if (target >= guide.step.instructionCount) then target = guide.step.instructionCount - 1
      jsRoutes.controllers.Guide.jumpTo(guide.step.index, target).ajax()
  )

  $(".graphical .carriage-type").link().text(machine, "carriage", (c) -> if c? then "Carriage #{c}" else "Carriage")
  $("#bar .progress-bar").carriageBar()
  $(".needle-labels").needleLabels(200)
  $(".needles.main").needles(200)
  $(".needles.double").needles(200, "needles", false)

  stepbar = $("#step-bar .progress-bar")
  stepbar.link().attr("aria-valuenow")(guide, "stepProgress")
  stepbar.link().width(guide, "stepProgress", "%")
  stepbar.find("span.sr-only").link().text(guide, "stepProgress")

  $(".instruction-counter").link().switchClass("hidden")(guide, "step", (s) -> not (s? and s.instructionCount > 1))
  $(".instruction-count").link().text(guide, "instructionsRemaining")
  $(".instruction-count").link().switchClass("hidden")(guide, "instructionsRemaining", (r) -> r == 1)
  $(".instruction-label").link().switchClass("hidden")(guide, "instructionsRemaining", (r) -> r == 1)
  $(".instruction-last").link().switchClass("hidden")(guide, "instructionsRemaining", (r) -> r != 1)

  guide.bind("step:change", (_, step) ->
    speed = 200
    $(".previous-step").each(->
      c = $(this)
      if (c.data("step") < step.index) then c.slideDown(speed)
      else c.slideUp(speed)
    )
    $(".next-step").each(->
      c = $(this)
      if (c.data("step") > step.index) then c.slideDown(speed)
      else c.slideUp(speed)
    )
  )

  $(".current-step-number").link().text(guide, "step", (s) -> if s? then s.index + 1 else "")
  $(".step-number-total").link().text(guide, "planInfo", (p) -> if p? then p.totalSteps else "")

  $(".needles.main").link().data("needles")(guide, "instruction", (instruction) ->
    if not instruction? then return ""
    state =
      if instruction.markNeedlesMainBed.length > 0 || instruction.markNeedlesDoubleBed.length > 0
        instruction.stateAfter
      else
        instruction.stateBefore
    for c, i in state.needles
      if instruction.markNeedlesMainBed.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
  )
  $(".needles.double").link().data("needles")(guide, "instruction", (instruction) ->
    if not instruction? then return ""
    state =
      if instruction.markNeedlesMainBed.length > 0 || instruction.markNeedlesDoubleBed.length > 0
        instruction.stateAfter
      else
        instruction.stateBefore
    for c, i in state.doubleBedNeedles
      if instruction.markNeedlesDoubleBed.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
  )

  $("#instruction-text").link().text(guide, "instruction", (i) -> if i? then i.text else "")

  makeKCarriage($(".kcarriage"))
  makeDoubleBedCarriage($(".doublebedcarriage"))

  $("#btn-output-3d").click(->
    $("#btn-output-2d").addClass("btn-default")
    $("#btn-output-2d-dual").addClass("btn-default")
    $("#btn-output-3d").removeClass("btn-default")

    $(".outputs").empty()
    output = $("<div></div>").appendTo($(".outputs"))
    output.addClass("output-3d").addClass("output")
    output.knitted3d()
    output.link().data("visibleStitches")(guide, "instruction", (instruction) ->
      if instruction? then instruction.stateBefore.visibleStitches3D
      else 0
    )
  )
  $("#btn-output-2d").click(->
    $("#btn-output-2d").removeClass("btn-default")
    $("#btn-output-2d-dual").addClass("btn-default")
    $("#btn-output-3d").addClass("btn-default")

    $(".outputs").empty()
    output = $("<div></div>").appendTo($(".outputs"))
    output.addClass("output-2d").addClass("output")
    output.knitted2d("knitted")
    output.link().data("knitted")(guide, "instruction", (i) -> if i? then i.stateBefore.output else undefined)
  )
  $("#btn-output-2d-dual").click(->
    $("#btn-output-2d").addClass("btn-default")
    $("#btn-output-2d-dual").removeClass("btn-default")
    $("#btn-output-3d").addClass("btn-default")

    $(".outputs").empty()
    output = $("<div></div>").appendTo($(".outputs"))
    output.addClass("output-2d-dual").addClass("output")
    front = $("<div></div>").appendTo(output)
    front.addClass("output-2d-front")
    front.knitted2d("knitted")
    front.link().data("knitted")(guide, "instruction", (i) -> if i? then i.stateBefore.output else undefined)
    back = $("<div></div>").appendTo(output)
    back.addClass("output-2d-back")
    back.knitted2d("knitted")
    back.link().data("knitted")(guide, "instruction", (i) -> if i? then i.stateBefore.doubleBedOutput else undefined)
  )
  $("#btn-output-2d").click()

  Leap.loop({enableGestures: true}, (frame) ->
    for gesture in frame.gestures
      switch gesture.type
        when "keyTap"
          console.debug("LeapMotion detected 'next' gesture (keyTap)")
          jsRoutes.controllers.Guide.next().ajax()
        when "screenTap"
          console.debug("LeapMotion detected 'next' gesture (keyTap)")
          jsRoutes.controllers.Guide.previous().ajax()
  )
)

makeKCarriage = (elem) ->
  canvasJ = $("<canvas style='width: 130; height: 60'></canvas>")
  canvasJ.appendTo(elem)
  canvas = canvasJ.get(0)
  ctx = canvas.getContext("2d")

  drawKCarriage = (elem, settings) ->
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
      drawUnpressedButtonAt(36)
    if settings.tuckRight
      ctx.fillRect(48, 46, 10, 3)
    else
      drawUnpressedButtonAt(48)

    if settings.mc
      drawUnpressedButtonAt(60)
      ctx.fillRect(60, 46, 10, 3)
    else if settings.l
      ctx.fillRect(60, 46, 10, 3)
    else
      drawUnpressedButtonAt(60)

    if settings.partLeft
      ctx.fillRect(72, 46, 10, 3)
    else
      drawUnpressedButtonAt(72)
    if settings.partRight
      ctx.fillRect(84, 46, 10, 3)
    else
      drawUnpressedButtonAt(84)

  drawUnpressedButtonAt = (x) ->
    ctx.beginPath()
    ctx.rect(x, 46, 10, 5)
    ctx.stroke()

  guide.bind("step:change", (_, step) ->
    if step? then drawKCarriage($(".kcarriage"), step.stateAfter.carriage.k)
  )

makeDoubleBedCarriage = (elem) ->
  canvasJ = $("<canvas style='width: 130; height: 60'></canvas>")
  canvasJ.appendTo(elem)
  canvas = canvasJ.get(0)
  ctx = canvas.getContext("2d")

  drawDoubleBedCarriage = (elem, settings) ->
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

    drawLines(13, 25)
    drawLines(101, 25)

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
      drawLines(57, 38)
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

  drawLines = (startX, offsetY) ->
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

  guide.bind("step:change", (_, step) ->
    if step? then drawDoubleBedCarriage($(".doublebedcarriage"), step.stateAfter.carriage.doubleBed)
  )
