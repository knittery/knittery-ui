###
  3D representation of a knitting. Use data-knitted="<json>" to set the values to display.

  Usage: $(".knitting").knitted3d()
###

define(["jquery", "threejs", "lib/trackball-controls", "2drender"], ($, THREE, TrackballControls, stitchRenderer) ->
  makeTextures = (data, stitchWidth = 5) ->
    thicknessInStiches = 10 # TODO figure that out from the model itself
    canvas = document.createElement("canvas")
    if data? and data.length > 0
      size = stitchRenderer.sizeOf(data)
      ctx = canvas.getContext("2d")
      aspectRatio = 0.8
      stitchHeight = stitchWidth * aspectRatio
      canvas.width = size.stitches * stitchWidth
      canvas.height = size.rows * stitchHeight
      renderMetadata = stitchRenderer.drawStitches(data, ctx, aspectRatio, stitchWidth)

      rowOfMark = (mark) ->
        rawIndex = i for e, i in data when e[0].marks and e[0].marks.indexOf(mark) >= 0
        renderMetadata.originalRowToRow(rawIndex)

      subimage = (startRow, endRow, startStitch, endStitch, rotate = 0) ->
        startX = renderMetadata.originalStitchToCoordinates(startStitch + renderMetadata.stitchOffset)
        endX = renderMetadata.originalStitchToCoordinates(endStitch + 1 + renderMetadata.stitchOffset)
        startY = renderMetadata.originalRowToCoordinates(endRow)
        endY = renderMetadata.originalRowToCoordinates(startRow)
        cnv = document.createElement("canvas")
        cnv.width = endX - startX
        cnv.height = endY - startY
        cnvCtx = cnv.getContext("2d")
        cnvCtx.save()
        cnvCtx.translate(cnv.width / 2, cnv.height / 2)
        cnvCtx.rotate(Math.PI * rotate / 180) if rotate is not 0
        cnvCtx.drawImage(canvas, startX, startY, cnv.width, cnv.height,
          -cnv.width / 2, -cnv.height / 2, cnv.width, cnv.height)
        cnvCtx.restore()
        cnv

      inside = (height) ->
        cnv = document.createElement("canvas")
        cnv.width = 30 * stitchWidth
        cnv.height = 30 * stitchWidth
        cnvCtx = cnv.getContext("2d")
        cnvCtx.fillStyle = "#515050"
        cnvCtx.fillRect(0, 0, cnv.width, cnv.height)
        cnv

      combineX = (imageA, imageB) ->
        cnv = document.createElement("canvas")
        cnv.width = imageA.width + imageB.width
        cnv.height = Math.max(imageA.height, imageB.height)
        cnvCtx = cnv.getContext("2d")
        cnvCtx.drawImage(imageA, 0, 0)
        cnvCtx.drawImage(imageB, imageA.width, 0)
        cnv

      frontBackIndex = rowOfMark("front/back")
      backLashIndex = rowOfMark("back/lash")
      forSides = Math.round(thicknessInStiches / 2)
      forSidesRows = Math.round(thicknessInStiches / 2) #approx..

      frontStart = 0
      frontEnd = frontBackIndex - forSides
      backStart = frontBackIndex + forSides + 1
      backEnd = backLashIndex
      lashStart = backLashIndex + 1
      lashEnd = data.length
      sideHeight = frontEnd - frontStart
      totalWidth = renderMetadata.stitches
      coverStart = forSides
      coverEnd = totalWidth - forSides

      front: subimage(frontStart, frontEnd, coverStart, coverEnd)
      back: subimage(backStart, backEnd, coverStart, coverEnd, 180)
      lash: subimage(lashStart, lashEnd, coverStart, coverEnd, 180)
      left: combineX(
        subimage(frontStart, frontEnd, 0, forSides),
        subimage(backStart, backStart + sideHeight, 0, forSides))
      right: combineX(
        subimage(frontStart, frontEnd, coverEnd + 1, totalWidth),
        subimage(backStart, backStart + sideHeight, coverEnd + 1, totalWidth))
      bottom: subimage(frontEnd + 1, backStart - 1, coverStart, coverEnd, 90)
      inside: inside()
      full: canvas
    else
#     Empty textures
      front: canvas
      back: canvas
      lash: canvas
      left: canvas
      right: canvas
      bottom: canvas
      inside: canvas
      full: canvas

  sum = (a, b) -> a + b

  composeSingleTexture = (textures) ->
    parts = [
      {texture: textures.left, width: 25},
      {texture: textures.front, width: 500},
      {texture: textures.right, width: 25},
      {texture: textures.back, width: 500},
      {texture: textures.bottom, width: 25},
      {texture: textures.lash, width: 500},
      {texture: textures.inside, width: 25}
    ]

    canvas = $("<canvas style='width: 100%; height: 100%'></canvas>").get(0)
    canvas.heigh = 700
    canvas.width = (p.width for p in parts).reduce(sum)
    ctx = canvas.getContext("2d")
    drawPart = (part, width) ->
      ctx.drawImage(part, 0, 0, part.width, part.height, 0, 0, width, canvas.height)
    ctx.save()
    for part in parts
      drawPart(part.texture, part.width)
      ctx.translate(part.width, 0)
    ctx.restore()
    canvas

  $.fn.extend({
    knitted3d: (dataName = "knitted") -> this.each(->
      root = $(this)
      canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
      canvasJ.appendTo(root)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")

      createModel = () ->
        data = if root.data(dataName)? then root.data(dataName) else []
        txts = makeTextures(data)
        txt = composeSingleTexture(txts)
        canvas.width = txt.width
        canvas.height = txt.height
        ctx.drawImage(txt, 0, 0, txt.width, txt.height)

      root.bind(dataName + ":data", () -> createModel())

      createModel()
    )

    knitted3dTexture: (dataName = "knitted", stitchWidth = 10) -> this.each(->
      root = $(this)
      canvasJ = $("<canvas style='width: 100%; height: 100%'></canvas>")
      canvasJ.appendTo(root)
      canvas = canvasJ.get(0)
      ctx = canvas.getContext("2d")

      createModel = ->
        data = if root.data(dataName)? then root.data(dataName) else []
        if (data.length > 0)
          textures = makeTextures(data, stitchWidth)
          txt = composeSingleTexture(textures)
          canvas.width = txt.width
          canvas.height = txt.height
          ctx.drawImage(txt, 0, 0, txt.width, txt.height)

      root.bind(dataName + ":data", () -> createModel())
      createModel()
    )

    knitted3dModel: (dataName = "knitted") -> this.each(->
      root = $(this)
      scene = new THREE.Scene()
      scene.fog = new THREE.FogExp2(0x000000, 0.035)
      scene.add(new THREE.AmbientLight(0xcccccc))
      pointLight = new THREE.PointLight(0xff4444, 5, 30)
      pointLight.position.set(5, 0, 0)
      scene.add(pointLight)

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

      model = undefined
      textures = undefined
      added = false

      updateModel = -> if model? and textures?
        texture = new THREE.Texture(composeSingleTexture(textures))
        texture.needsUpdate = true
        model.material.map = texture
        scene.add(model) unless added
        added = true

      loader = new THREE.JSONLoader()
      loader.load("assets/models/laptop.json", (geometry, materials) ->
        model = new THREE.Mesh(geometry, materials[0])
        model.geometry.computeBoundingBox()
        size = model.geometry.boundingBox.max.clone().sub(model.geometry.boundingBox.min)
        model.position.copy(size.divideScalar(4).negate())
        updateModel()
      )

      createModel = ->
        data = if root.data(dataName)? then root.data(dataName) else []
        if (data.length > 0)
          textures = makeTextures(data)
          updateModel()

      root.bind(dataName + ":data", () -> createModel())
      createModel()
    )
  })
)