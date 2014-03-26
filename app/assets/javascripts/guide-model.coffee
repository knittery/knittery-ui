# Step { name, description, number }

guide = $().model("currentStep", "currentInstruction")

started = false

guide.start = (route) -> if (!started)
  me = this
  ws = new ReconnectingWebSocket(route.webSocketURL())
  ws.onmessage = (msg) ->
    parsed = $.parseJSON(msg.data)
    me.currentStep = parsed.step
    me.currentInstruction = parsed.instruction
  started = true

window.guide = guide
window.guide.start(jsRoutes.controllers.Guide.subscribe())
