###
  3D representation (fetched from server).

  Usage: $(".knitting").knitted3d()
###

define(["jquery", "threejs", "lib/trackball-controls"], ($, THREE, TrackballControls, graphing) ->
  $.fn.extend({
    knitted3d: () -> this.each(->
      root = $(this)

      #TODO
    )
  })
)