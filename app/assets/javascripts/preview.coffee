area = 2000

$(() ->
  graph = new Graph()

  elem = $("#preview-canvas")
  renderer = initRenderer(elem)
  [camera, controls] = initCamera(renderer.domElement)
  scene = new THREE.Scene()
  
  animate = () ->
    requestAnimationFrame(animate)
    controls.update()
    render()

  lines = []
  render = () ->
    if graph.layout?
      graph.layout.step()
      (l.geometry.verticesNeedUpdate = true) for l in lines
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
      graph.layout = new SpringLayout(graph, new THREE.Vector3(area, area, area), 0.3, 5, 10)
      
      scene.add(nodeDrawObject(node)) for node in graph.nodes
      for edge in graph.edges
        e = edgeDrawObject(edge)
        lines.push(e)
        scene.add(e)
      render()
  }
)


loadGraph = (data, graph) ->
  graph.addNode(n.id, {colors: n.colors})           for n in data.nodes
  graph.addEdge(graph.node(e.n1), graph.node(e.n2)) for e in data.edges
  

nodeDrawObject = (node) ->
  color = node.data.colors[0]
  material = new THREE.MeshBasicMaterial({ color: color })
  geometry = new THREE.SphereGeometry(25, 25, 25)
  mesh = new THREE.Mesh(geometry, material)
  mesh.position.x = Math.floor(Math.random() * area - area/2)
  mesh.position.y = Math.floor(Math.random() * area - area/2)
  mesh.position.z = Math.floor(Math.random() * area - area/2)
  mesh.id = node.id
  node.data.drawObject = mesh
  node.position = mesh.position
  mesh

intersection = (a, b) ->
  [a, b] = [b, a] if a.length > b.length
  value for value in a when value in b

edgeDrawObject = (edge) ->
  from = edge.node1
  to = edge.node2
  color = intersection(from.data.colors, to.data.colors)[0]
  material = new THREE.LineBasicMaterial({ color: color, linewidth: 0.5 })
  geo = new THREE.Geometry()
  geo.vertices.push(from.data.drawObject.position)
  geo.vertices.push(to.data.drawObject.position)
  line = new THREE.Line(geo, material, THREE.LinePieces)
  line.scale.x = line.scale.y = line.scale.z = 1
  line.originalScale = 1
  line
   
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
