###
  3D representation of a knitting. Use data-knitted="<json>" to set the values to display.

  Usage: $(".knitting").knitted3d()
###

define(["jquery", "threejs", "lib/trackball-controls", "2drender"], ($, THREE, TrackballControls, stitchRenderer) ->
  textures = (data) ->
    canvas = document.createElement("canvas")
    if data? and data.length > 0
      size = stitchRenderer.sizeOf(data)
      ctx = canvas.getContext("2d")
      aspectRatio = 0.8
      stitchWidth = 20
      stitchHeight = stitchWidth * aspectRatio
      canvas.width = size.stitches * stitchWidth
      canvas.height = size.rows * stitchHeight
      renderMetadata = stitchRenderer.drawStitches(data, ctx, aspectRatio, stitchWidth)

      rowOfMark = (mark) ->
        rawIndex = i for e, i in data when e[0].marks and e[0].marks.indexOf(mark) >= 0
        renderMetadata.originalRowToRow(rawIndex)

      subimage = (startRow, endRow) ->
        start = renderMetadata.originalRowToCoordinates(endRow)
        end = renderMetadata.originalRowToCoordinates(startRow)
        cnv = document.createElement("canvas")
        cnv.width = renderMetadata.stitches * stitchWidth
        cnv.height = end - start
        cnvCtx = cnv.getContext("2d")
        cnvCtx.drawImage(canvas, 0, start, cnv.width, cnv.height, 0, 0, cnv.width, cnv.height)
        cnv

      frontBackIndex = rowOfMark("front/back")
      backLashIndex = rowOfMark("back/lash")

      front: subimage(0, frontBackIndex)
      back: subimage(frontBackIndex + 1, backLashIndex)
      lash: subimage(backLashIndex, data.length)
    else
#     Empty textures
      front: canvas
      back: canvas
      lash: canvas

  $.fn.extend({
    knitted3d: (dataName = "knitted") -> this.each(->
      root = $(this)
      canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
      canvasJ.appendTo(root)
      canvas = canvasJ.get(0)
      canvas.width = 1000
      canvas.height = canvas.width / canvasJ.width() * canvasJ.height()
      ctx = canvas.getContext("2d")

      createModel = () ->
        data = if root.data(dataName)? then root.data(dataName) else []
        txt = textures(data)
        updateImage = () ->
          ctx.clearRect(0, 0, canvas.width, canvas.height)
          ctx.drawImage(txt.front, 0, 0, txt.front.width, txt.front.height, 0, 0, 300,
            300 / txt.front.width * txt.front.height)
          ctx.drawImage(txt.back, 0, 0, txt.back.width, txt.back.height, 350, 0, 300,
            300 / txt.back.width * txt.back.height)
          ctx.drawImage(txt.lash, 0, 0, txt.lash.width, txt.lash.height, 700, 0, 300,
            300 / txt.lash.width * txt.lash.height)
        updateImage()

      root.bind(dataName + ":data", () -> createModel())

      createModel()
    )

    knitted3dModel: (dataName = "knitted") -> this.each(->
      root = $(this)

      loader = new THREE.JSONLoader()
      loader.load("assets/models/laptop.json", (geometry, materials) ->
        model = new THREE.Mesh(geometry, materials[0])

        scene = new THREE.Scene()
        scene.fog = new THREE.FogExp2(0x000000, 0.035)
        scene.add(new THREE.AmbientLight(0xcccccc))
        pointLight = new THREE.PointLight(0xff4444, 5, 30)
        pointLight.position.set(5, 0, 0)
        scene.add(pointLight)

        model.geometry.computeBoundingBox()
        size = model.geometry.boundingBox.max.clone().sub(model.geometry.boundingBox.min)
        model.position.copy(size.divideScalar(4).negate())
        scene.add(model)

        camera = new THREE.PerspectiveCamera(50, root.width() / root.height(), 1, 2000)
        camera.position.x = 2
        camera.position.y = 2
        camera.position.z = 2

        renderer = new THREE.WebGLRenderer({alpha: true})
        renderer.setPixelRatio(window.devicePixelRatio)
        renderer.setSize(root.width(), root.height())
        root.append(renderer.domElement)

        controls = new TrackballControls(camera);
        controls.rotateSpeed = 1.0
        controls.zoomSpeed = 1.2
        controls.panSpeed = 0.8
        controls.noZoom = false
        controls.noPan = false
        controls.staticMoving = true
        controls.dynamicDampingFactor = 0.3
        controls.keys = [65, 83, 68]

        onResize = () ->
          renderer.setSize(root.width(), root.height())
          camera.aspect = root.width() / root.height()
          camera.updateProjectionMatrix()
          controls.handlerResize()
        root.resize(onResize)

        render = () ->
          camera.lookAt(scene.position)
          renderer.render(scene, camera)
        animate = () ->
          requestAnimationFrame(animate)
          controls.update()
          render()

        controls.addEventListener('change', render)
        animate()
      )
    )
  })
)