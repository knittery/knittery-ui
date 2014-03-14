### Shows the 3d model of the knitting. ###
jQuery.fn.extend({
  knitted3d: (nodeSize = 0, showMesh = true) -> this.each(->
    root = $(this)
    renderer = initRenderer(root)
    [camera, controls] = initCamera(renderer.domElement)
    scene = setupScene()
    animate = () ->
      requestAnimationFrame(animate)
      controls.update()
    render = () -> renderer.render(scene, camera)
    controls.addEventListener("change", render)

    jsRoutes.controllers.Preview.json().ajax {
      success: (data) ->
        console.debug("Received 3d data from server. Loading now..")
        graph = new Graph()
        loadGraph(data, graph)

        for node in graph.nodes
          node.position.x = node.data.initialPosition.x
          node.position.y = node.data.initialPosition.y
          node.position.z = node.data.initialPosition.z

        console.debug("Generating the model from the 3d data..")
        sceneControl = new AggregateSceneControls()
        sceneControl.add(drawNodeEdge(graph, scene, nodeSize))
        if (showMesh)
          sceneControl.add(drawMesh(graph, scene))
        console.info("3d model loaded")

        updateVisibleStitches = () ->
          limit = root.data("visibleStitches")
          if not limit? then limit = graph.nodes.length
          sceneControl.showStitchesUpTo(limit)
        root.bind("visibleStitches:data", ->
          updateVisibleStitches()
          render()
        )

        updateVisibleStitches()
        animate() #start animation
    }

    root.data("camera", camera)
    render() #draw empty scene, content will be added later by the ajax callback
  )

  camera: -> $(this).data("camera")
})

# Aggregates SceneControls
class AggregateSceneControls
  constructor: -> @members = []
  add: (sceneControl) ->
    @members.push(sceneControl)
  showStitchesUpTo: (limit) ->
    m.showStitchesUpTo(limit) for m in @members
  layoutChanged: ->
    m.layoutChanged() for m in @members

### Make the graph from the json data received from the server. ###
loadGraph = (data, graph) ->
  for n in data.nodes
    gn = graph.addNode(n.id, {colors: n.colors})
    gn.data.initialPosition = n.position
  for e in data.edges when e.n1 != e.n2
    graph.addEdge(graph.node(e.n1), graph.node(e.n2), e.weight, {color: e.color})


initRenderer = (inElement) ->
  elem = $(inElement)
  renderer = new THREE.WebGLRenderer({antialias: true})
  renderer.setSize(elem.width(), elem.height())
  $(renderer.domElement).appendTo(elem)
  renderer.setClearColor(0xffffff, 1)
  renderer

initCamera = (element) ->
  elem = $(element)
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

setupScene = ->
  scene = new THREE.Scene()
  light = new THREE.DirectionalLight(0xffffff, 0.5)
  light.position.set(0, 0, 5000)
  scene.add(light)
  light2 = new THREE.DirectionalLight(0xffffff, 0.5)
  light2.position.set(0, 0, -5000)
  scene.add(light2)
  scene


### Execute logic based on existence of objects (i.e. Meshes) at a certain knitting progress ###
class StitchBasedVisibility
  constructor: ->
    @data = {}
    @push = (index, obj...) ->
      if @data[index]? then @data[index].push(obj...)
      else @data[index] = obj
      this
  add: (obj, stitchIndex...) ->
    max = stitchIndex.sort((a, b) -> b - a)[0]
    @push(max, obj)
  mergeWith: (other) ->
    @push(i, os) for i, os of other.data
  processAt: (onVisible, onInvisible, limit) ->
    @foreach((o, i) -> if i < limit then onVisible(o) else onInvisible(o))
  foreach: (f) ->
    for i,os of @data
      f(o, i) for o in os
    this

### Draws the graph as spheres (nodes) and lines between the spheres (edges). ###
drawNodeEdge = (graph, scene, nodeSize) ->
  sbv = new StitchBasedVisibility()
  nodeDrawObject = (node, size) ->
    color = node.data.colors[0]
    material = new THREE.MeshBasicMaterial({ color: color })
    geometry = new THREE.SphereGeometry(size, size, size)
    mesh = new THREE.Mesh(geometry, material)
    mesh.position = node.position
    mesh.id = node.id
    node.data.drawObject = mesh
    sbv.add(mesh, node.id)
    mesh
  edgeDrawObject = (edge) ->
    material = new THREE.LineBasicMaterial({ color: edge.data.color, linewidth: 1 })
    geo = new THREE.Geometry()
    geo.vertices.push(edge.node1.position)
    geo.vertices.push(edge.node2.position)
    line = new THREE.Line(geo, material, THREE.LinePieces)
    line.scale.x = line.scale.y = line.scale.z = 1
    line.originalScale = 1
    sbv.add(line, edge.node1.id, edge.node2.id)
    line

  if nodeSize > 0 then scene.add(nodeDrawObject(node, nodeSize)) for node in graph.nodes
  scene.add(edgeDrawObject(edge)) for edge in graph.edges
  sceneControl =
    showStitchesUpTo: (limit) ->
      sbv.processAt(((o) -> o.visible = true), ((o) -> o.visible = false), limit)
    layoutChanged: ->
      sbv.foreach((o) -> o.geometry.verticesNeedUpdate = true)
  sceneControl


### Draws the graph as a mesh (surface between the stitches). ###
drawMesh = (graph, scene) ->
  nodeSbv = new StitchBasedVisibility()
  for node,i in graph.nodes
    node.data.vertice = i
    nodeSbv.add(node, i)

  # Find circular paths in the graph (with max length of 8 edges)
  circles = graph.findCircles(6)
  # Reduce found circles by only keeping those that don't overlap
  byEdge = {}
  for c in circles
    for e in c.edges
      ecs = byEdge[e.index]
      if ecs? then ecs.push(c)
      else byEdge[e.index] = ecs = [c]
    c.badness = -c.length()
  relevantCircles = []
  circles.sort((a, b) -> a.length() - b.length())
  for c in circles
    # remove this from byEdge
    byEdge[e.index].splice(byEdge[e.index].indexOf(c), 1) for e in c.edges
    if c.badness < 1   # only keep if not too many common edges with already chosen
      relevantCircles.push(c)
      # punish all others that have common edges with us
      ((o.badness++) for o in byEdge[e.index]) for e in c.edges
  console.debug("Found #{circles.length} circles in the graph and reduced it to #{relevantCircles.length} faces.")
  # Now prepare the faces
  faceSbv = new StitchBasedVisibility()
  for circle in relevantCircles
    nodes = circle.nodes
    for i in [1..nodes.length - 2]
      face = new THREE.Face3(nodes[0].data.vertice, nodes[i].data.vertice, nodes[i + 1].data.vertice)
      faceSbv.add(face, nodes[0].id, nodes[i].id, nodes[i + 1].id)

  material = new THREE.MeshLambertMaterial {
    color: 0x003090
    emissive: 0x404090
    side: THREE.DoubleSide
  }

  geo = undefined
  mesh = undefined
  makeGeometry = (limit) ->
    geo = new THREE.Geometry()
    nodeSbv.processAt(((n) -> geo.vertices.push(n.position)), (->), limit)
    faceSbv.processAt(((face) -> geo.faces.push(face)), (->), limit)
    geo.computeFaceNormals()
    geo.computeVertexNormals()
    scene.remove(mesh) if mesh?
    mesh = new THREE.Mesh(geo, material)
    scene.add(mesh)

  sceneControl =
    showStitchesUpTo: (limit) ->
      makeGeometry(limit)
    layoutChanged: ->
      geo.computeFaceNormals()
      geo.computeVertexNormals()
      geo.verticesNeedUpdate = true
      geo.normalsNeedUpdate = true
  sceneControl
