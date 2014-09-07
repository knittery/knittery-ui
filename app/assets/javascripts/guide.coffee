require(["jquery", "guide-model", "machine", "leapjs", "binding", "needles", "carriage", "2dview", "3dview"], ($, guide, machine, Leap) ->
  $(() ->
    $("#next").link().disabled(guide, "step", (s) -> s? && s.last)
    $("#next").click(() ->
      jsRoutes.controllers.Guide.next().ajax()
      false
    )
    $("#prev").link().disabled(guide, "step", (s) -> s? && s.first)
    $("#prev").click(() ->
      jsRoutes.controllers.Guide.previous().ajax()
      false
    )
    $("#first").link().disabled(guide, "step", (s) -> s? && s.first)
    $("#first").click(() ->
      jsRoutes.controllers.Guide.first().ajax()
      false
    )
    $("#last").click(() ->
      jsRoutes.controllers.Guide.last().ajax()
      false
    )
    $("#last").link().disabled(guide, "step", (s) -> s? && s.last)

    $(document).keydown((e) -> switch e.which
      when 37 #left
        jsRoutes.controllers.Guide.previousInstruction().ajax()
        e.preventDefault()
      when 39 #right
        jsRoutes.controllers.Guide.nextInstruction().ajax()
        e.preventDefault()
    )

    $("#nextInstruction").link().disabled(guide, "instruction", (i) -> i? && i.last)
    $("#nextInstruction").click(() ->
      jsRoutes.controllers.Guide.nextInstruction().ajax()
      false
    )
    $("#prevInstruction").link().disabled(guide, "instruction", (i) -> i? && i.first)
    $("#prevInstruction").click(() ->
      jsRoutes.controllers.Guide.previousInstruction().ajax()
      false
    )
    $(".step-list li:not(.special-step)").each(() ->
      url = "#" + $(this).data("step")
      $(this).wrapInner("<a href=" + url + ">")
    )
    $(".step-list li a").click(() ->
      jsRoutes.controllers.Guide.jumpTo($(this).attr("href").substr(1)).ajax()
      false
    )
    $(".instruction-count").click(() ->
      target = prompt("Jump to instruction", guide.instruction.index)
      if (target?)
        if (target < 0) then target = 0
        else if (target >= guide.step.instructionCount) then target = guide.step.instructionCount - 1
        jsRoutes.controllers.Guide.jumpTo(guide.step.index, target).ajax()
    )

    $(".graphical .carriage-type").link().text(machine, "carriage", (c) -> if c? then "Carriage #{c}" else "Carriage")
    $("#bar .progress-bar").carriageBar()
    $(".needle-labels").needleLabels(200)
    $(".needles.main").needles(200)
    $(".needles.double").needles(200, "needles", false)

    stepbar = $("#step-bar .progress-bar")
    stepbar.link().attr("aria-valuenow")(guide, "stepProgress")
    stepbar.link().width(guide, "stepProgress", "%")
    stepbar.find("span.sr-only").link().text(guide, "stepProgress")

    $(".instruction-counter").link().switchClass("hidden")(guide, "step", (s) -> not (s? and s.instructionCount > 1))
    $(".instruction-count").link().text(guide, "instructionsRemaining")
    $(".instruction-count").link().switchClass("hidden")(guide, "instructionsRemaining", (r) -> r == 1)
    $(".instruction-label").link().switchClass("hidden")(guide, "instructionsRemaining", (r) -> r == 1)
    $(".instruction-last").link().switchClass("hidden")(guide, "instructionsRemaining", (r) -> r != 1)

    guide.bind("step:change", (_, step) ->
      speed = 200
      $(".previous-step").each(->
        c = $(this)
        if (c.data("step") < step.index) then c.slideDown(speed)
        else c.slideUp(speed)
      )
      $(".next-step").each(->
        c = $(this)
        if (c.data("step") > step.index) then c.slideDown(speed)
        else c.slideUp(speed)
      )
    )

    $(".current-step-number").link().text(guide, "step", (s) -> if s? then s.index + 1 else "")
    $(".step-number-total").link().text(guide, "planInfo", (p) -> if p? then p.totalSteps else "")

    $(".needles.main").link().data("needles")(guide, "instruction", (instruction) ->
      if not instruction? then return ""
      state =
        if instruction.markNeedlesMainBed.length > 0 || instruction.markNeedlesDoubleBed.length > 0
          instruction.stateAfter
        else
          instruction.stateBefore
      for c, i in state.needles
        if instruction.markNeedlesMainBed.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
    )
    $(".needles.double").link().data("needles")(guide, "instruction", (instruction) ->
      if not instruction? then return ""
      state =
        if instruction.markNeedlesMainBed.length > 0 || instruction.markNeedlesDoubleBed.length > 0
          instruction.stateAfter
        else
          instruction.stateBefore
      for c, i in state.doubleBedNeedles
        if instruction.markNeedlesDoubleBed.indexOf(i) == -1 then c.toUpperCase() else c.toLowerCase()
    )

    $("#instruction-text").link().text(guide, "instruction", (i) -> if i? then i.text else "")

    $(".kcarriage").kCarriage()
    $(".kcarriage").link().data("settings")(guide, "step", (s) -> if s? then s.stateAfter.carriage.k else undefined )

    $(".doublebedcarriage").doubleBedCarriage()
    $(".doublebedcarriage").link().data("settings")(guide, "step", (s) -> if s? then s.stateAfter.carriage.doubleBed else undefined)

    $("#btn-output-3d").click(->
      $("#btn-output-2d").addClass("btn-default")
      $("#btn-output-2d-dual").addClass("btn-default")
      $("#btn-output-3d").removeClass("btn-default")

      $(".outputs").empty()
      output = $("<div></div>").appendTo($(".outputs"))
      output.addClass("output-3d").addClass("output")
      output.knitted3d()
      output.link().data("visibleStitches")(guide, "instruction", (instruction) ->
        if instruction? then instruction.stateBefore.visibleStitches3D
        else 0
      )
    )
    $("#btn-output-2d").click(->
      $("#btn-output-2d").removeClass("btn-default")
      $("#btn-output-2d-dual").addClass("btn-default")
      $("#btn-output-3d").addClass("btn-default")

      $(".outputs").empty()
      output = $("<div fit='full-width'></div>").appendTo($(".outputs"))
      output.addClass("output-2d").addClass("output")
      output.knitted2d()
      jsRoutes.controllers.KnittingResult.mainBed().ajax {
          success: (data) ->
            output.data("knitted", data.output)
            output.trigger("knitted:data")
        }
      output.link().attr("to-row")(guide, "instruction", (i) -> if i? then i.stateBefore.outputRow - 1 else 0)
    )
    $("#btn-output-2d-dual").click(->
      $("#btn-output-2d").addClass("btn-default")
      $("#btn-output-2d-dual").removeClass("btn-default")
      $("#btn-output-3d").addClass("btn-default")

      $(".outputs").empty()
      output = $("<div></div>").appendTo($(".outputs"))
      output.addClass("output-2d-dual").addClass("output")
      front = $("<div fit='full-width'></div>").appendTo(output)
      front.addClass("output-2d-front")
      front.knitted2d()
      jsRoutes.controllers.KnittingResult.mainBed().ajax {
        success: (data) ->
          front.data("knitted", data.output)
          front.trigger("knitted:data")
      }
      front.link().attr("to-row")(guide, "instruction", (i) -> if i? then i.stateBefore.outputRow - 1 else 0)

      back = $("<div fit='full-width'></div>").appendTo(output)
      back.addClass("output-2d-back")
      back.knitted2d()
      jsRoutes.controllers.KnittingResult.doubleBed().ajax {
        success: (data) ->
          back.data("knitted", data.output)
          back.trigger("knitted:data")
      }
      back.link().attr("to-row")(guide, "instruction", (i) -> if i? then i.stateBefore.doubleBedOutputRow - 1 else 0)
    )
    $("#btn-output-2d").click()

    Leap.loop({enableGestures: true}, (frame) ->
      for gesture in frame.gestures
        switch gesture.type
          when "keyTap"
            console.debug("LeapMotion detected 'next' gesture (keyTap)")
            jsRoutes.controllers.Guide.next().ajax()
          when "screenTap"
            console.debug("LeapMotion detected 'next' gesture (keyTap)")
            jsRoutes.controllers.Guide.previous().ajax()
    )
  )
)