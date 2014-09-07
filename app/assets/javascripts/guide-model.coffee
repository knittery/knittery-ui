###
  Model representation of the guide.
  Properties:
    - step: current step with {name, description, number, instructionCount}
    - instruction: current instruction with {number}
    - planInfo: info about the current plan with {totalSteps}
###

define(["jquery", "binding", "reconnecting-websocket"], ($, model, websocket) ->
  guide = model("step", "instruction", "planInfo")
  guide.derived("stepProgress", ["step", "planInfo"], (s, p) ->
    if s? and p? then s.index * 100 / (p.totalSteps - 1) else 0
  )
  guide.derived("instructionsRemaining", ["step", "instruction"], (s, i) ->
    if s? and i? then s.instructionCount - i.index else undefined
  )

  started = false

  guide.start = (route) -> if (!started)
    me = this
    ws = new websocket(route.webSocketURL())
    ws.onmessage = (msg) ->
      parsed = $.parseJSON(msg.data)
      me.step = parsed.step
      me.instruction = parsed.instruction
      me.planInfo = parsed.planInfo
    started = true

  window.guide = guide
  window.guide.start(jsRoutes.controllers.Guide.subscribe())

  guide
)