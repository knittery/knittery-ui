$(() ->
  active = $(".step.active") 
  active.prevAll(".step").
    removeClass("future").
    addClass("past")
  active.nextAll(".step").
    removeClass("past").
    addClass("future")
)