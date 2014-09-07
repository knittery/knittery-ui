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

  $("#btn-output-2d").click()
)

init2dPreview = (elem) ->
  elem.attr("fit", "knitted")
  elem.knitted2d()
  jsRoutes.controllers.KnittingResult.mainBed().ajax {
    success: (data) ->
      elem.data("knitted", data.output)
      elem.trigger("knitted:data")
  }
  Leap.loop(() ->)


init3dPreview = (elem) ->
  elem.knitted3d()
  cameraControls = initLeapCamera(elem)
  Leap.loop((frame) ->
    cameraControls.update(frame)
  )

initLeapCamera = (elem) ->
  cameraControls = new THREE.LeapCameraControls(elem.camera())
  cameraControls.rotateEnabled = true
  cameraControls.rotateSpeed = 3
  cameraControls.rotateHands = 1
  cameraControls.rotateFingers = [2, 3]
  cameraControls.zoomEnabled = true
  cameraControls.zoomSpeed = 2
  cameraControls.zoomHands = 1
  cameraControls.zoomFingers = [4, 5]
  cameraControls.panEnabled = false
  cameraControls