$(() ->
  $("#next").click(() ->
    jsRoutes.controllers.Guide.next().ajax()
    false
  )
  $("#prev").click(() ->
    jsRoutes.controllers.Guide.previous().ajax()
    false
  )

  $(".graphical .carriage-type").link().text(machine, "carriage", (c) -> if c? then "Carriage #{c}" else "Carriage")
  $("#bar .progress-bar").carriageBar()
  $(".needles").needles(200)

  totalSteps = -> +$(".step-number-total").text()

  guide.bind("currentStep:change", (_, step) ->
    $(".step.active").removeClass("active")
    active = $("#step-#{step.number}")
    active.addClass("active").removeClass("future").removeClass("past")
    active.prevAll(".step").
      removeClass("future").
      addClass("past")
    active.nextAll(".step").
      removeClass("past").
      addClass("future")
  )
  $(".current-step-number").link().text(guide, "currentStep", (s) -> if s? then s.number else "")

  $("#next").link().disabled(guide, "currentStep", (s) -> s? && s.number == totalSteps())
  $("#prev").link().disabled(guide, "currentStep", (s) -> s? && s.number == 1)

  $(".needles").link().data("needles")(guide, "currentStep", (s) ->
    if not s? then return ""
    for c, i in s.stateAfter.needles
      if c == s.stateBefore.needles[i] then c.toUpperCase() else c.toLowerCase()
  )
)
