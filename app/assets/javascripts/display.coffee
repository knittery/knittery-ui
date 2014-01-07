$(() ->
  req = jsRoutes.controllers.Display.positions().ajax({
    success: (data) ->
      window.machineEvents.publish("positionChange", {carriage: c, position: p}) for c, p of data.positions
      setRow(data.row)
      setNeedles(data.patternRow)
  })

  ws = new WebSocket(jsRoutes.controllers.Display.subscribe().webSocketURL())
  ws.onmessage = (msg) ->
    parsed = $.parseJSON(msg.data)
    updateFrom(parsed)

  $("#bar .progress-bar").carriageBar()
  $(".graphical .needle-pos").carriagePosition(false)
  $(".graphical .carriage-type").currentCarriageType()
  $("#K-position .positions-value").carriagePosition(true, "K")
  $("#L-position .positions-value").carriagePosition(true, "L")
  $("#G-position .positions-value").carriagePosition(true, "G")

  window.machineEvents.start(jsRoutes.controllers.Display.subscribe())
)

updateFrom = (msg) ->
  switch msg.event
    when "positionChange"
      setRow(msg.row)
    when "needlePatternUpdate"
      setNeedles(msg.patternRow)

setRow = (row) ->
  $(".row-position .positions-value").text(row)

setNeedles = (patternRow) ->
  $(".needles").data("needles", patternRow)
  $(".needles").trigger("updated")
  
$(() ->
  $(".needles").needles(200)
)
