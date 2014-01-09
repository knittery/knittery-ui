$(() ->
  $("#bar .progress-bar").carriageBar()
  $(".graphical .needle-pos").carriagePosition(false)
  $(".graphical .carriage-type").link().text(machine, "carriage", (c) ->
    if c? then "Carriage #{c}" else "Carriage")
  $("#K-position .positions-value").carriagePosition(true, "K")
  $("#L-position .positions-value").carriagePosition(true, "L")
  $("#G-position .positions-value").carriagePosition(true, "G")
  $(".row-position .positions-value").link().text(machine, "row")
  
  $(".needles").needles(200).
    link().data("needles")(machine, "needles")

  jsRoutes.controllers.Display.positions().ajax({
    success: (data) ->
      window.machineEvents.publish("positionChange", {carriage: c, position: p, row: data.row}) for c, p of data.positions
      window.machineEvents.publish("needlePatternUpdate", {patternRow: data.patternRow})
  })
  window.machineEvents.start(jsRoutes.controllers.Display.subscribe())
  window.machine.start(jsRoutes.controllers.Display.subscribe())
)