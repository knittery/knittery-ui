### Shows the 3d model of the knitting. ###
jQuery.fn.extend({
  knitted3d: (nodeSize = 0, showMesh = true) -> this.each(->
    root = $(this)
    renderer = initRenderer(root)
    [camera, controls] = initCamera(renderer.domElement)
    scene = setupScene(camera)
    knittingAnimator = undefined

    animate = () ->
      requestAnimationFrame(animate)
      knittingAnimator.update() if knittingAnimator?
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

        knitUpToFromData = () ->
          limit = root.data("visibleStitches")
          if limit? then limit else graph.nodes.length
        knittingAnimator = new KnittingAnimator(sceneControl, knitUpToFromData(), render)
        updateVisibleStitches = () ->
          knittingAnimator.continious(knitUpToFromData())
        root.bind("visibleStitches:data", updateVisibleStitches)

        updateVisibleStitches()
        knittingAnimator.update()
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

class KnittingAnimator
  ### speed =  ms to go to target ###
  constructor: (@sceneControl, initial = 0, @render, @speed = 300) ->
    @last = -1
    @lastTime = 0
    @current = initial
    @target = initial
    @lastTarget = @target
  instant: (to) -> if @target != to
    @target = to
    @lastTarget = @to
    @lastTime = new Date().getTime()
    @current = to
  continious: (to) -> if @target != to
    @lastTarget = @target
    @lastTime = new Date().getTime()
    @target = to
  update: ->
    if @current != @target
      delta = (@target - @lastTarget) / @speed * (new Date().getTime() - @lastTime)
      if (delta >= @target - @lastTarget) then @current = @target
      else @current = @lastTarget + delta
    if @current != @last
      @sceneControl.showStitchesUpTo(Math.round(@current))
      @last = @current
      @render()


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

setupScene = (camera) ->
  scene = new THREE.Scene()
  ambientLight = new THREE.AmbientLight(0x333333)
  scene.add(ambientLight)
  light = new THREE.DirectionalLight(0xdfebff, 0.7)
  light.position = camera.position
  scene.add(light)
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

usedColors = (graph) ->
  colorMap = {}
  (colorMap[e.data.color] = true) for e in graph.edges
  colors = []
  colors.push(c) for c of colorMap
  colors

# finds circles in the graph that should be displayed as surfaces.
findSurfaces = (graph, maxSize = 6) ->
  # Find circular paths in the graph (with max length of 8 edges)
  circles = graph.findCircles(maxSize)
  # Reduce found circles by only keeping those that don't overlap
  byEdge = {}
  for c in circles
    for e in c.edges
      ecs = byEdge[e.index]
      if ecs? then ecs.push(c)
      else byEdge[e.index] = ecs = [c]
    c.badness = -c.length()
  surfaces = []
  circles.sort((a, b) -> a.length() - b.length())
  for c in circles
    # remove this from byEdge
    byEdge[e.index].splice(byEdge[e.index].indexOf(c), 1) for e in c.edges
    if c.badness < 1   # only keep if not too many common edges with already chosen
      surfaces.push(c)
      # punish all others that have common edges with us
      ((o.badness++) for o in byEdge[e.index]) for e in c.edges
  surfaces

surfaceColor = (surface) ->
  colors = {}
  for edge in surface.edges
    if colors[edge.data.color]? then colors[edge.data.color]++
    else colors[edge.data.color] = 1
  best = undefined
  bestCount = 0
  for color, count of colors when count > bestCount
    best = color
    bestCount = count
  best

### Draws the graph as a mesh (surface between the stitches). ###
drawMesh = (graph, scene) ->
  nodeSbv = new StitchBasedVisibility()

  #Materials
  colors = usedColors(graph)
  materials = for color in colors
    new THREE.MeshLambertMaterial {
      color: color
      side: THREE.DoubleSide
    }
  material = new THREE.MeshFaceMaterial(materials)

  #Vertices
  for node,i in graph.nodes
    node.data.vertice = i
    nodeSbv.add(node, i)

  #Faces
  surfaces = findSurfaces(graph)
  console.debug("Found #{surfaces.length} surfaces in the graph.")
  faceSbv = new StitchBasedVisibility()
  for surface in surfaces
    nodes = surface.nodes
    colorIndex = colors.indexOf(surfaceColor(surface))
    for i in [1..nodes.length - 2]
      face = new THREE.Face3(nodes[0].data.vertice, nodes[i].data.vertice, nodes[i + 1].data.vertice)
      face.materialIndex = colorIndex
      faceSbv.add(face, nodes[0].id, nodes[i].id, nodes[i + 1].id)

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
