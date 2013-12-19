$("input#send").click(() ->
  data = $("input#toSend").val()
  req = jsRoutes.controllers.SerialSimulator.send().ajax({
    data: data
    contentType: "text/plain"
    dataType: "text"
  })
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
