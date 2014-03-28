# Step { name, description, number }

guide = $().model("step", "instruction", "planInfo")
guide.derived("stepProgress", ["step", "planInfo"], (s, p) ->
  if s? and p? then s.index * 100 / (p.totalSteps - 1) else 0
)

started = false

guide.start = (route) -> if (!started)
  me = this
  ws = new ReconnectingWebSocket(route.webSocketURL())
  ws.onmessage = (msg) ->
    parsed = $.parseJSON(msg.data)
    me.step = parsed.step
    me.instruction = parsed.instruction
    me.planInfo = parsed.planInfo
  started = true

window.guide = guide
window.guide.start(jsRoutes.controllers.Guide.subscribe())
