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
  makeKCarriage($(".kcarriage"))

  $(".output-3d").knitted3d()

  Leap.loop({enableGestures: true}, (frame) ->
    for gesture in frame.gestures
      switch gesture.type
        when "keyTap" then $("#next").click()
        when "screenTap" then $("#prev").click()
  )
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


makeKCarriage = (elem) ->
  canvasJ = $("<canvas style='width: 30; height: 30'></canvas>")
  canvasJ.appendTo(elem)
  canvas = canvasJ.get(0)
  ctx = canvas.getContext("2d")

  drawKCarriage = (elem, settings) ->
    ctx.clearRect(0, 0, 200, 100)
    ctx.save()
    
    ctx.fillStyle = "grey"
    ctx.strokeStyle="grey";
    ctx.font="6px Arial";  

    ctx.beginPath();
    ctx.lineWidth="1";
    ctx.rect(0,0,120,45); 
    ctx.stroke();
 
    ctx.beginPath();
    ctx.lineWidth="1";
    ctx.rect(10, 33, 15, 5); 
    ctx.stroke(); 
    
    if settings.holdingCamLever is "N"
      ctx.fillRect(10, 33, 5, 5)
      ctx.fillText("N", 10, 28);
    else if settings.holdingCamLever is "H"
      ctx.fillRect(15, 33, 5, 5)
      ctx.fillText("H", 15, 28);
    else
      ctx.fillRect(20, 33, 5, 5)
      ctx.fillText("I", 15, 28);
    
    if settings.tuckLeft
      ctx.fillRect(36, 46, 10, 3)
    else
      drawButtons(36)
    if settings.tuckRight
      ctx.fillRect(48, 46, 10, 3)
    else
      drawButtons(48)

    if settings.mc
      drawButtons(60)
      ctx.fillRect(60, 46, 10, 3)
    else if settings.l
      ctx.fillRect(60, 46, 10, 3)
    else
      drawButtons(60)

    if settings.partLeft
      ctx.fillRect(72, 46, 10, 3)
    else
      drawButtons(72)
    if settings.partRight
      ctx.fillRect(84, 46, 10, 3)
    else
      drawButtons(84)

  drawButtons = (x) ->
    ctx.beginPath();
    ctx.lineWidth="1";
    ctx.rect(x, 46, 10, 5); 
    ctx.stroke();
      
  guide.bind("currentStep:change", (_, step) ->
    if step? then drawKCarriage($(".kcarriage"), step.stateAfter.kCarriageSetting)
  )
