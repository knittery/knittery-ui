send = (data) ->
  req = jsRoutes.controllers.SerialSimulator.send().ajax({
    data: data
    contentType: "text/plain"
    dataType: "text"
  })

$("input#send").click(() ->
  stopAuto
  send($("input#toSend").val())
  $("input#toSend").select()
  false
)

$(".send-templates a").click(() ->
  send($(this).data("value"))
)


autoRunning = false
autoTimer = 0
autoPos = 0
autoDirection = 0
stopAuto = () ->
  window.clearInterval(autoTimer) if autoRunning
  autoRunning = false

$(".automatic a.start").click(() ->
  stopAuto
  
  speed = $(this).data("speed")
  carriage = $(this).data("carriage")
  min = $(this).data("min")
  max = $(this).data("max")
  handle = () ->
    if (autoPos >= max) then autoDirection = -1
    else if (autoPos <= min) then autoDirection = 1
    autoPos = autoPos + autoDirection
    dir = if autoDirection == -1 then "<-" else "->"
    [cp, needle] = switch autoPos
      when autoPos < 0 then ["<", 0]
      when autoPos > 199 then [">", 199]
      else ["_", autoPos]
    send("@#{needle}\t#{autoPos}\t#{dir}\t#{carriage}\t#{cp}")

  autoRunning = true
  autoTimer = window.setInterval(handle, speed)
  false
)

$(".automatic a.stop").click(() ->
  stopAuto()
  false
)


$(() ->
  log = $("#log")
  ws = new WebSocket(jsRoutes.controllers.SerialSimulator.subscribe().webSocketURL())
  ws.onmessage = (msg) ->
    value = htmlEncode(msg.data)
    log.html(value + "<br/>" + log.html())
)

htmlEncode = (html) ->
  document.createElement("a").appendChild(document.createTextNode(html)).parentNode.innerHTML
