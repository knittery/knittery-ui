$(() ->
  log = $("#log")
  ws = new WebSocket(jsRoutes.controllers.Display.subscribe().webSocketURL())
  ws.onmessage = (msg) ->
    log.text(msg.data)
)
