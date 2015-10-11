THREE = require('three')
TrackballControls = require('three.trackball')

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

  createControl = (camera) ->
    controls = new TrackballControls(camera);
    controls.rotateSpeed = 1.0
    controls.zoomSpeed = 1.2
    controls.panSpeed = 0.8
    controls.noZoom = false
    controls.noPan = false
    controls.staticMoving = true
    controls.dynamicDampingFactor = 0.3
    controls.keys = [65, 83, 68]
    controls

  loader = new THREE.JSONLoader()

  m.directive('knittingView3d', ($window) ->
    scope:
      knitting: '='
      modelUrl: '='

    link: (scope, elem) ->
      [scene, camera, renderer] = createScene()
      controls = createControl(camera)
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

      scope.$watch('modelUrl', (url) ->
        if url?
          loader.load(url, (geometry, materials) -> scope.$apply(->
            model = new THREE.Mesh(geometry, materials[0])
            scene.add(model)
          )))
  )
