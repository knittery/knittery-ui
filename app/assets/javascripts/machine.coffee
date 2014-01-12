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
    when "left" then "-#{p.overlap} left"
    when "right" then "-#{p.overlap} right"
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

  ###
    Updates the content to a textual representation of the carriage.
  ###
  carriagePosition: (detailed, carriage) ->
    root = $(this)
    window.machineEvents.subscribe("positionChange", (event, msg) ->
      if not carriage? or carriage == msg.carriage 
        position = msg.position
        value = if detailed
          [x, text] = interpretPosition(position)
          text
        else
          switch position.where
            when "needles" then position.needle
            else position.where
        root.text(value)
    )

  ###
    Shows a needle board that is connected to the machine
  ###
  machineNeedles: () ->
    root = $(this)
    root.needles(200)
    window.machineEvents.subscribe("needlePatternUpdate", (event, msg) ->
      root.data("needles", msg.patternRow)
      root.trigger("updated")
    )
})

interpretPosition = (position) ->
  if not position? then return undefined
  switch position.where
    when "left"    then [0, "-#{position.overlap} left"]
    when "right"   then [100, "-#{position.overlap} right"]
    when "needles" then [(position.index+0.5)*100/200, position.needle]
    else ""