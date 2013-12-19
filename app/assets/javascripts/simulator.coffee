send = (data) ->
  req = jsRoutes.controllers.SerialSimulator.send().ajax({
    data: data
    contentType: "text/plain"
    dataType: "text"
  })

$("input#send").click(() ->
  send($("input#toSend").val())
  $("input#toSend").select()
  false
)

$(".send-templates a").click(() ->
  send($(this).data("value"))
)

$(() ->
  log = $("#log")
  ws = new WebSocket(jsRoutes.controllers.SerialSimulator.subscribe().webSocketURL())
  ws.onmessage = (msg) ->
    value = htmlEncode(msg.data)
    log.html(log.html() + "<br/>" + value)
)

htmlEncode = (html) ->
  document.createElement("a").appendChild(document.createTextNode(html)).parentNode.innerHTML
