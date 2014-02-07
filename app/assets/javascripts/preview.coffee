$(() ->
  graph = new Graph()
  nodeSize = 20

  elem = $("#preview-canvas")
  renderer = initRenderer(elem)
  [camera, controls] = initCamera(renderer.domElement)
  scene = new THREE.Scene()
  
  animate = () ->
    requestAnimationFrame(animate)
    controls.update()
    render()

  updateScene = ->
  render = () ->
    if graph.layout?
      graph.layout.step() for i in [1..5]
      updateScene()
    renderer.render(scene, camera)
  
  controls.addEventListener("change", render)
  onWindowResize = () ->
    camera.aspect = elem.width() / elem.height()
    camera.updateProjectionMatrix()
    renderer.setSize(elem.width(), elem.height())
    controls.handleResize()
    render()
  window.addEventListener("resize", onWindowResize, false)
  
  animate()

  jsRoutes.controllers.Preview.json().ajax {
    success: (data) ->
      loadGraph(data, graph)
      
      area = 2000
      for node in graph.nodes
        node.position.x = Math.floor(Math.random() * area - area/2)
        node.position.y = Math.floor(Math.random() * area - area/2)
        node.position.z = Math.floor(Math.random() * area - area/2)
      graph.layout = new SpringLayout(graph, new THREE.Vector3(area, area, area), 1, 1/5)

      updateScene = drawNodeEdge(graph, scene, nodeSize)
      render()
  }
)


loadGraph = (data, graph) ->
  for n in data.nodes
    graph.addNode(n.id, {colors: n.colors})
  for e in data.edges
    graph.addEdge(graph.node(e.n1), graph.node(e.n2), e.weight, {color: e.color})


drawNodeEdge = (graph, scene, nodeSize) ->
  nodeDrawObject = (node, size) ->
    color = node.data.colors[0]
    material = new THREE.MeshBasicMaterial({ color: color })
    geometry = new THREE.SphereGeometry(size, size, size)
    mesh = new THREE.Mesh(geometry, material)
    mesh.position = node.position
    mesh.id = node.id
    node.data.drawObject = mesh
    mesh
  edgeDrawObject = (edge) ->
    material = new THREE.LineBasicMaterial({ color: edge.data.color, linewidth: 0.5 })
    geo = new THREE.Geometry()
    geo.vertices.push(edge.node1.position)
    geo.vertices.push(edge.node2.position)
    line = new THREE.Line(geo, material, THREE.LinePieces)
    line.scale.x = line.scale.y = line.scale.z = 1
    line.originalScale = 1
    line

  if nodeSize > 0
    scene.add(nodeDrawObject(node, nodeSize)) for node in graph.nodes
  lines = []
  for edge in graph.edges
    e = edgeDrawObject(edge)
    lines.push(e)
    scene.add(e)
  -> (l.geometry.verticesNeedUpdate = true) for l in lines


initRenderer = (inElement) ->
  renderer = new THREE.WebGLRenderer({antialias: true})
  renderer.setSize(inElement.width(), inElement.height())
  $(renderer.domElement).appendTo(inElement)
  renderer.setClearColor(0xffffff, 1) 
  renderer

initCamera = (elem1) ->
  elem = $(elem1)
  camera = new THREE.PerspectiveCamera(40, elem.width() / elem.height(), 1, 1000000)
  camera.position.z = 5000
  controls = new THREE.TrackballControls(camera)
  controls.rotateSpeed = 0.5
  controls.zoomSpeed = 5.2
  controls.panSpeed = 1
  controls.staticMoving = false
  controls.dynamicDampingFactor = 0.3
  controls.keys = [65, 83, 68]
  [camera, controls]
