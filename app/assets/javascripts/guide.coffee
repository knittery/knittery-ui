$(() ->
  active = $(".step.active") 
  active.prevAll(".step").
    removeClass("future").
    addClass("past")
  active.nextAll(".step").
    removeClass("past").
    addClass("future")

  $(".graphical .carriage-type").currentCarriageType()
  $("#bar .progress-bar").carriageBar()
  $(".needles").machineNeedles()

  jsRoutes.controllers.Display.positions().ajax({
    success: (data) ->
      window.machineEvents.publish("positionChange", {carriage: c, position: p, row: data.row}) for c, p of data.positions
      window.machineEvents.publish("needlePatternUpdate", {patternRow: data.patternRow})
  })
  window.machineEvents.start(jsRoutes.controllers.Display.subscribe())

  window.guideEvents.start(jsRoutes.controllers.Guide.subscribe())
)