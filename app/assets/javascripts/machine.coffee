###
Event Types:
  - positionChange
      - position
      - carriage
Don't forget to start
###
MachineEvents = {
  subscribe: (event, fn) -> $(this).bind(event, fn)
  unsubscribe: (event, fn) -> $(this).unbind(event, fn)
  publish: (event, data) -> $(this).trigger(event, data)

  started: false
  start: (route) -> if (!started)
    me = this
    ws = new WebSocket(route.webSocketURL())
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
    window.machineEvents.subscribe("positionChange", (event, msg) ->
      [needlePercentage, text] = interpretPosition(msg.position)
      color = switch msg.carriage
        when "K" then "info"
        when "L" then "success"
        when "G" then "warning"
      bar.removeClass("progress-bar-#{c}") for c in ["info", "warning", "success"]
      bar.addClass("progress-bar-#{color}")
      bar.attr("aria-valuenow", needlePercentage)
      bar.width(needlePercentage + "%")
      bar.find("span.sr-only").text(text)
    )

  ###
    Updates the content to the name of the current carriage.
  ###
  currentCarriageType: () ->
    root = $(this)
    window.machineEvents.subscribe("positionChange", (event, msg) ->
      root.text("Carriage #{msg.carriage}")
    )

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
})


interpretPosition = (position) -> switch position.where
  when "left"    then [0, "-#{position.overlap} left"]
  when "right"   then [100, "-#{position.overlap} right"]
  when "needles" then [(position.index+0.5)*100/200, position.needle]
  else ""