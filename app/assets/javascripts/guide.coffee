$(() ->
  $("#next").click(() ->
    jsRoutes.controllers.Guide.next().ajax()
    false
  )
  $("#prev").click(() ->
    jsRoutes.controllers.Guide.previous().ajax()
    false
  )

  $(".graphical .carriage-type").link().text(machine, "carriage", (c) -> if c? then "Carriage #{c}" else "Carriage")
  $("#bar .progress-bar").carriageBar()
  $(".needles.main").needles(200)
  $(".needles.double").needles(200, "needles", false)

  totalSteps = -> +$(".step-number-total").text()

  guide.bind("currentStep:change", (_, step) ->
    $(".step.active").removeClass("active")
    active = $("#step-#{step.number}")
    active.addClass("active").removeClass("future").removeClass("past")
    active.prevAll(".step").
      removeClass("future").
      addClass("past")
    active.nextAll(".step").
      removeClass("past").
      addClass("future")
  )
  $(".current-step-number").link().text(guide, "currentStep", (s) -> if s? then s.number else "")

  $("#next").link().disabled(guide, "currentStep", (s) -> s? && s.number == totalSteps())
  $("#prev").link().disabled(guide, "currentStep", (s) -> s? && s.number == 1)

  $(".needles.main").link().data("needles")(guide, "currentStep", (s) ->
    if not s? then return ""
    for c, i in s.stateAfter.needles
      if s.manualNeedles.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
  )
  $(".needles.double").link().data("needles")(guide, "currentStep", (s) ->
    if not s? then return ""
    for c, i in s.stateAfter.doubleBedNeedles
      if s.manualDoubleBedNeedles.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
  )

  makeOutput($(".output"))
)

makeOutput = (elem) ->
  stitchHeight = 2
  stitchWidth = 2
  maxRows = 50

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
            ctx.fillRect(0, 0, stitchWidth, stitchHeight/2)
            ctx.fillStyle = "#000000"
            ctx.fillRect(0, stitchHeight/2, stitchWidth, stitchHeight/2)
          when "castOff"
            ctx.fillStyle = "#000000"
            ctx.fillRect(0, 0, stitchWidth, stitchHeight/2)
            ctx.fillStyle = stitch.yarns[0].color
            ctx.fillRect(0, stitchHeight/2, stitchWidth, stitchHeight/2)
        ctx.translate(stitchWidth, 0)
      ctx.translate(-w, stitchHeight)
    ctx.translate(0, -h)
    ctx.restore()

  guide.bind("currentStep:change", (_, step) ->
    if step? then drawOutput($(".output"), step.stateBefore.output)
  )