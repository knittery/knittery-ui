$(() ->
  $("#bar .progress-bar").carriageBar()
  $(".graphical .needle-pos").link().text(machine, "positionText")
  $(".graphical .carriage-type").link().text(machine, "carriage", (c) ->
    if c? then "Carriage #{c}" else "Carriage")

  showPosition = (c) -> (pos) ->
    p = pos[c]
    if p? then switch p.where
      when "left" then "-#{p.overlap} left"
      when "right" then "-#{p.overlap} right"
      when "needles" then p.needle
      when "removed" then "-"
      when true then p.where
    else "unknown"
  $("#"+c+"-position .positions-value").link().text(machine, "positions", showPosition(c)) for c in ["K", "L", "G"]

  $(".row-position .positions-value").link().text(machine, "row")
  
  $(".needles").needles(200).
    link().data("needles")(machine, "needles")
)