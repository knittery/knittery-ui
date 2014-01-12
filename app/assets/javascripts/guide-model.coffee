# Step { name, description, number }

guide = $().model("currentStep")

guide.start = (route) -> if (!started)
  me = this
  ws = new ReconnectingWebSocket(route.webSocketURL())
  ws.onmessage = (msg) ->
    parsed = $.parseJSON(msg.data)
    me.currentStep = parsed.step
  started = true

window.guide = guide
window.guide.start(jsRoutes.controllers.Guide.subscribe())
