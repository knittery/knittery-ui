require(["jquery", "leapjs", "threejs", "lib/LeapCameraControls", "2dview", "3dview"], ($, Leap, THREE, LeapCameraControls) ->
  init2dPreview = (elem) ->
    elem.attr("fit", "knitted")
    elem.knitted2d(true)
    jsRoutes.controllers.KnittingResult.mainBed().ajax {
      success: (data) ->
        elem.data("knitted", data.output)
        elem.trigger("knitted:data")
    }
    Leap.loop(() ->)

  init3dPreview = (elem) ->
    elem.knitted3dTexture()
#    elem.knitted3dModel()
    jsRoutes.controllers.KnittingResult.mainBed().ajax {
      success: (data) ->
        elem.data("knitted", data.output)
        elem.trigger("knitted:data")
    }

  $(() ->
    $("#btn-output-3d").click(->
      $("#btn-output-2d").addClass("btn-default")
      $("#btn-output-2d-dual").addClass("btn-default")
      $("#btn-output-3d").removeClass("btn-default")

      $(".previews").empty()
      preview = $("<div></div>").appendTo($(".previews"))
      preview.addClass("preview-3d").addClass("preview")
      init3dPreview(preview)
    )
    $("#btn-output-2d").click(->
      $("#btn-output-2d").removeClass("btn-default")
      $("#btn-output-2d-dual").addClass("btn-default")
      $("#btn-output-3d").addClass("btn-default")

      $(".previews").empty()
      preview = $("<div></div>").appendTo($(".previews"))
      preview.addClass("preview-2d").addClass("preview")
      init2dPreview(preview)
    )

    $("#btn-output-3d").click()
  )
)