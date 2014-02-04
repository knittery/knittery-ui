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
      graph.layout.generate()
      for l in lines
        l.geometry.verticesNeedUpdate = true
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
      graph.layout = new Layout.ForceDirected(graph, {
        width: 1000
        height: 1000
        iterations: 100000000
        layout: "3d"
      })
      graph.layout.init()
      
      scene.add(nodeDrawObject(node)) for node in graph.nodes
      for edge in graph.edges
        e = edgeDrawObject(edge.source, edge.target)
        lines.push(e)
        scene.add(e)
      render()
  }
)


loadGraph = (data, graph) ->
  nodes = {}
  for nodeData in data.nodes
    node = new Node(nodeData.id)
    node.data.colors = nodeData.colors
    nodes[node.id] = node 
  graph.addNode(node) for id, node of nodes
  graph.addEdge(nodes[e.n1], nodes[e.n2]) for e in data.edges
  graph
  

nodeDrawObject = (node) ->
  color = node.data.colors[0]
  material = new THREE.MeshBasicMaterial({ color: color })
  geometry = new THREE.SphereGeometry(25, 25, 25)
  mesh = new THREE.Mesh(geometry, material)
  area = 5000
  mesh.position.x = Math.floor(Math.random() * (area + area + 1) - area)
  mesh.position.y = Math.floor(Math.random() * (area + area + 1) - area)
  mesh.position.z = Math.floor(Math.random() * (area + area + 1) - area)
  mesh.id = node.id
  node.data.drawObject = mesh
  node.position = mesh.position
  mesh

edgeDrawObject = (from, to) ->
  material = new THREE.LineBasicMaterial({ color: 0x000000, linewidth: 0.5 })
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
