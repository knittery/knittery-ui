THREE = require('three')
TrackballControls = require('three.trackball')
render = require('./stitch-render')
EffectiveKnittingArea = require('./knitting-areas').EffectiveKnittingArea
MarkedArea = require('./knitting-areas').MarkedArea


module.exports = (m) ->

  createScene = ->
    scene = new THREE.Scene()
    scene.add(new THREE.AmbientLight(0xcccccc))
    scene.fog = new THREE.FogExp2(0x000000, 0.035)
    pointLight = new THREE.PointLight(0xff4444, 5, 30)
    pointLight.position.set(5, 0, 0)
    scene.add(pointLight)
    camera = new THREE.PerspectiveCamera(50, 1, 1, 2000)
    camera.position.x = 2
    camera.position.y = 2
    camera.position.z = 2
    renderer = new THREE.WebGLRenderer({alpha: true})
    renderer.setPixelRatio(window.devicePixelRatio)
    [scene, camera, renderer]

  createControl = (camera, elem) ->
    controls = new TrackballControls(camera, elem);
    controls.rotateSpeed = 1.0
    controls.zoomSpeed = 1.2
    controls.panSpeed = 0.8
    controls.noZoom = false
    controls.noPan = false
    controls.staticMoving = true
    controls.dynamicDampingFactor = 0.3
    controls.keys = [65, 83, 68]
    controls


  makeTextureCanvas = (knitting) ->
    canvas = document.createElement("canvas")
    ctx = canvas.getContext("2d")

    stitchSize = 10
    data = render.parseJson(knitting, stitchSize)
    effective = new EffectiveKnittingArea(data.mainBed)
    front = new MarkedArea(effective.rows, null, 'front/back')
    back = new MarkedArea(effective.rows, 'front/back', 'back/lash')
    lash = new MarkedArea(effective.rows, 'back/lash')

    toPixels = (stitches) -> stitches * stitchSize
    draw = render.renderStitches(ctx, stitchSize)
    canvas.width = toPixels(front.width() + back.width() + lash.width())
    canvas.height = toPixels(Math.max(front.height(), back.height(), lash.height()))
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    ctx.save()
    draw(front)
    ctx.translate(toPixels(front.width()), 0)
    draw(back)
    ctx.translate(toPixels(back.width()), 0)
    draw(lash)
    ctx.restore()
    canvas

  loader = new THREE.JSONLoader()


  # shows 3d model of the resulting product using Three.js
  m.directive('knittingView3d', ($window) ->
    scope:
      knitting: '='
      modelUrl: '='

    link: (scope, elem) ->
      [scene, camera, renderer] = createScene()
      controls = createControl(camera, elem[0])
      elem.append(renderer.domElement)

      resize = ->
        renderer.setSize(elem.width(), elem.height())
        camera.aspect = elem.width() / elem.height()
        camera.updateProjectionMatrix()
        controls.handleResize()
      $($window).resize(resize)
      resize()

      render = ->
        camera.lookAt(scene.position)
        renderer.render(scene, camera)
      animate = ->
        $window.requestAnimationFrame(animate)
        controls.update()
        render()
      animate()

      modelAdded = false
      texture = null
      model = null

      updateModel = ->
        if scope.knitting?
          texture = new THREE.Texture(makeTextureCanvas(scope.knitting))
          texture.needsUpdate = true
          model.material.map = texture
          if not modelAdded
            scene.add(model)
            modelAdded = true

      scope.$watch('knitting', updateModel)
      scope.$watch('modelUrl', (url) ->
        if url?
          loader.load(url, (geometry, materials) -> scope.$apply(->
            model = new THREE.Mesh(geometry, materials[0])
          ))
        else
          model = null
        updateModel()
      )
  )

  # Shows the texture that will be used for the 3d model
  m.directive('knittingView3dTexture', ($window) ->
    replace: true
    scope:
      knitting: '='

    template: """
      <canvas style="width: 100%; height: height: 100%"></canvas>
      """

    link: (scope, elem) ->
      canvas = elem[0]
      ctx = canvas.getContext("2d")
      canvas.width = 1
      canvas.height = 1
      draw = () ->
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        if scope.knitting
          cvs = makeTextureCanvas(scope.knitting)
          canvas.width = cvs.width
          canvas.height = cvs.height
          ctx.drawImage(cvs, 0, 0)

      scope.$watch('knitting', draw)
      $($window).resize(draw)
  )
