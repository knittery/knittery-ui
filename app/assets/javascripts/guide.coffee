$(() ->
  $("#next").click(() ->
    jsRoutes.controllers.Guide.next().ajax()
    false
  )
  $("#prev").click(() ->
    jsRoutes.controllers.Guide.previous().ajax()
    false
  )

  $(".graphical .carriage-type").currentCarriageType()
  $("#bar .progress-bar").carriageBar()
  $(".needles").needles(200)

  jsRoutes.controllers.Display.positions().ajax({
    success: (data) ->
      window.machineEvents.publish("positionChange", {carriage: c, position: p, row: data.row}) for c, p of data.positions
      window.machineEvents.publish("needlePatternUpdate", {patternRow: data.patternRow})
  })
  window.machineEvents.start(jsRoutes.controllers.Display.subscribe())

  window.guideEvents.start(jsRoutes.controllers.Guide.subscribe())
  window.guideEvents.subscribe("change", (_, msg) ->
    nr = msg.step.number
    first = nr == 1
    last = nr == +$(".step-number-total").text()
    
    $(".step.active").removeClass("active")
    $("#step-#{nr}").addClass("active").removeClass("future").removeClass("past")
    $(".current-step-number").text(nr)
    updateButtonState(first, last)
    updateSteps()
    updateNeedles(msg.step.stateBefore.needles, msg.step.stateAfter.needles, true)
  )
  updateSteps()
)

updateSteps = () ->
  active = $(".step.active") 
  active.prevAll(".step").
    removeClass("future").
    addClass("past")
  active.nextAll(".step").
    removeClass("past").
    addClass("future")

updateButtonState = (first, last) ->
  if (last) then $("#next").attr("disabled", "disabled")
  else $("#next").removeAttr("disabled")
  if (first) then $("#prev").attr("disabled", "disabled")
  else $("#prev").removeAttr("disabled")

updateNeedles = (before, after, color) ->
  state = if color
    for c, i in after
      if c == before[i] then c.toUpperCase() else c.toLowerCase()
  else after
  $(".needles").data("needles", state).trigger("updated")