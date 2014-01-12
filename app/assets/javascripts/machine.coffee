machine = $().model("positions", "carriage", "row", "needles")
machine.positions = {}
machine.derived("position", ["positions", "carriage"], (p, c) -> p[c])
machine.derived("positionPercentage", "position", (p) ->
  if p? then switch p.where
    when "left" then 0
    when "right" then 100
    when "needles" then (p.index+0.5)*100/200
  else undefined
)
machine.derived("positionText", "position", (p) ->
  if p? then switch p.where
    when "left" then "left"
    when "right" then "right"
    when "needles" then p.needle
  else undefined
)

machine.start = (route) ->
  return if this.started
  me = this
  ws = new ReconnectingWebSocket(route.webSocketURL())
  ws.onmessage = (data) ->
    msg = $.parseJSON(data.data)
    switch msg.event
      when "positionChange"
        me.carriage = msg.carriage
        me.row = msg.row
        pos = me.positions
        pos[msg.carriage] = msg.position
        me.positions = pos
      when "needlePatternUpdate"
        me.needles = msg.patternRow
  @started = true
  machine

window.machine = machine
window.machine.start(jsRoutes.controllers.Display.subscribe())

jQuery.fn.extend({
  ###
    Shows the carriage position as a progress bar.
  ###
  carriageBar: () ->
    bar = $(this)
    # Color of the bar
    carriageClasses =
      K: "info"
      L: "success"
      G: "warning"
    for crg, clz of carriageClasses
      bar.link().switchClass(clz)(machine, "carriage", (c) -> c == crg)
    # Position
    bar.link().attr("aria-valuenow")(machine, "positionPercentage")
    bar.link().width(machine, "positionPercentage", "%")
    bar.find("span.sr-only").link().text(machine, "positionText")
})