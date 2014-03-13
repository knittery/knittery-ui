$(() ->
  canvas = $("#preview-canvas")
  canvas.knitted3d(0)
  cameraControls = initLeapCamera(canvas)

  Leap.loop((frame) ->
    cameraControls.update(frame)
  )
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