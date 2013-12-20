$(() ->
  log = $("#log")
  ws = new WebSocket(jsRoutes.controllers.Display.subscribe().webSocketURL())
  ws.onmessage = (msg) ->
    parsed = $.parseJSON(msg.data)
    updateFrom(parsed)
)

updateFrom = (msg) ->
  if (msg.event == "positionChange")
    setPosition(msg.carriage, msg.position)
  
setPosition = (carriage, position) ->
  elem = $("#"+carriage+"-position .carriage-value")
  value = switch position.where
    when "left"    then "-#{position.overlap} left"
    when "right"   then "-#{position.overlap} right"
    when "needles" then position.needle
    else ""
  elem.text(value)
