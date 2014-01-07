$(() ->
  $("#bar .progress-bar").carriageBar()
  $(".graphical .needle-pos").carriagePosition(false)
  $(".graphical .carriage-type").currentCarriageType()
  $("#K-position .positions-value").carriagePosition(true, "K")
  $("#L-position .positions-value").carriagePosition(true, "L")
  $("#G-position .positions-value").carriagePosition(true, "G")
  $(".row-position .positions-value").rowNumber()
  
  $(".needles").machineNeedles()

  jsRoutes.controllers.Display.positions().ajax({
    success: (data) ->
      window.machineEvents.publish("positionChange", {carriage: c, position: p, row: data.row}) for c, p of data.positions
      window.machineEvents.publish("needlePatternUpdate", {patternRow: data.patternRow})
  })
  window.machineEvents.start(jsRoutes.controllers.Display.subscribe())
)