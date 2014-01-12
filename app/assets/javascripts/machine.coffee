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
        me.positions[msg.carriage] = msg.position
      when "needlePatternUpdate"
        me.needles = msg.patternRow
  @started = true
  machine

window.machine = machine

###
Event Types:
  - positionChange
      - position
      - carriage
      - row
  - needlePatternUpdate
      - patternRow
Don't forget to start
###
MachineEvents = {
  subscribe: (event, fn) -> $(this).bind(event, fn)
  unsubscribe: (event, fn) -> $(this).unbind(event, fn)
  publish: (event, data) -> $(this).trigger(event, data)

  started: false
  start: (route) -> if (!started)
    me = this
    ws = new ReconnectingWebSocket(route.webSocketURL())
    ws.onmessage = (msg) ->
      parsed = $.parseJSON(msg.data)
      me.publish(parsed.event, parsed)
    started = true
}

window.machineEvents = MachineEvents

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