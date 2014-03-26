$(() ->
  $("#next").link().disabled(guide, "currentStep", (s) -> s? && s.last)
  $("#next").click(() ->
    jsRoutes.controllers.Guide.next().ajax()
    false
  )
  $("#prev").link().disabled(guide, "currentStep", (s) -> s? && s.first)
  $("#prev").click(() ->
    jsRoutes.controllers.Guide.previous().ajax()
    false
  )
  $("#first").link().disabled(guide, "currentStep", (s) -> s? && s.first)
  $("#first").click(() ->
    jsRoutes.controllers.Guide.first().ajax()
    false
  )
  $("#last").click(() ->
    jsRoutes.controllers.Guide.last().ajax()
    false
  )
  $("#last").link().disabled(guide, "currentStep", (s) -> s? && s.last)


  $(".graphical .carriage-type").link().text(machine, "carriage", (c) -> if c? then "Carriage #{c}" else "Carriage")
  $("#bar .progress-bar").carriageBar()
  $(".needles.main").needles(200)
  $(".needles.double").needles(200, "needles", false)

  guide.bind("currentStep:change", (_, step) ->
    $(".step.active").removeClass("active")
    active = $("#step-#{step.index}")
    active.addClass("active").removeClass("future").removeClass("past")
    active.prevAll(".step").
    removeClass("future").
    addClass("past")
    active.nextAll(".step").
    removeClass("past").
    addClass("future")
  )
  $(".current-step-number").link().text(guide, "currentStep", (s) -> if s? then s.index + 1 else "")

  $(".needles.main").link().data("needles")(guide, "currentInstruction", (instruction) ->
    if not instruction? then return ""
    for c, i in instruction.stateAfter.needles
      if instruction.markNeedlesMainBed.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
  )
  $(".needles.double").link().data("needles")(guide, "currentInstruction", (instruction) ->
    if not instruction? then return ""
    for c, i in instruction.stateAfter.doubleBedNeedles
      if instruction.markNeedlesDoubleBed.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
  )

  makeOutput($(".output-2d"))

  $(".output-3d").knitted3d()
  $(".output-3d").link().data("visibleStitches")(guide, "currentStep", (s) ->
    if s? then s.stateBefore.visibleStitches3D
    else 0
  )

  $("#btn-output-3d").click(->
    $(".output-2d").hide()
    $(".output-3d").show()
    $("#btn-output-2d").addClass("btn-default")
    $("#btn-output-3d").removeClass("btn-default")
  )
  $("#btn-output-2d").click(->
    $(".output-2d").show()
    $(".output-3d").hide()
    $("#btn-output-2d").removeClass("btn-default")
    $("#btn-output-3d").addClass("btn-default")
  )

  Leap.loop({enableGestures: true}, (frame) ->
    for gesture in frame.gestures
      switch gesture.type
        when "keyTap"
          console.debug("LeapMotion detected 'next' gesture (keyTap)")
          jsRoutes.controllers.Guide.next().ajax()
        when      "screenTap"
          console.debug("LeapMotion detected 'next' gesture (keyTap)")
          jsRoutes.controllers.Guide.previous().ajax()
  )
)

makeOutput = (elem) ->
  stitchHeight = 2
  stitchWidth = 2
  maxRows = 200

  canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
  canvasJ.appendTo(elem)
  canvas = canvasJ.get(0)
  w = canvas.width = stitchWidth * 200
  h = canvas.height = stitchHeight * maxRows
  ctx = canvas.getContext("2d")

  drawOutput = (elem, output) ->
    ctx.clearRect(0, 0, w, h)
    ctx.save()
    for row, i in output.reverse()
      if i >= maxRows then break
      for stitch in row
        switch stitch.type
          when "plain"
            ctx.fillStyle = stitch.yarns[0].color
            ctx.fillRect(0, 0, stitchWidth, stitchHeight)
          when "purl"
            ctx.fillStyle = stitch.yarns[0].color
            ctx.fillRect(0, 0, stitchWidth, stitchHeight)
          when "castOn"
            ctx.fillStyle = stitch.yarns[0].color
            ctx.fillRect(0, 0, stitchWidth, stitchHeight / 2)
            ctx.fillStyle = "#000000"
            ctx.fillRect(0, stitchHeight / 2, stitchWidth, stitchHeight / 2)
          when "castOff"
            ctx.fillStyle = "#000000"
            ctx.fillRect(0, 0, stitchWidth, stitchHeight / 2)
            ctx.fillStyle = stitch.yarns[0].color
            ctx.fillRect(0, stitchHeight / 2, stitchWidth, stitchHeight / 2)
        ctx.translate(stitchWidth, 0)
      ctx.translate(-w, stitchHeight)
    ctx.translate(0, -h)
    ctx.restore()

  guide.bind("currentStep:change", (_, step) ->
    if step? then drawOutput($(".output"), step.stateBefore.output)
  )