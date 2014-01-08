###
Event Types:
  - change
      -step: Step

Types: 
  - Step
      - name
      - description
      - number
Don't forget to start
###
GuideEvents = {
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

window.guideEvents = GuideEvents
