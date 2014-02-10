Vector = THREE.Vector3

class Node
  constructor: (@id, @data) ->
    @position = new Vector()
    @neighbors = []
    @edges = []
  addEdge: (edge) ->
    @edges.push(edge)
    @neighbors.push(edge.other(this))

class Edge
  constructor: (@node1, @node2, @weight, @data) ->
    if @node1 == @node2 then throw "Edge on only one node"
  other: (node) ->
    if @node1 == node then @node2
    else if @node2 == node then @node1
    else throw "edge #{@node1.id}-#{@node2.id} does not contain #{node.id}"
  nodes: -> [@node1, @node2]
  vector: -> @node1.position.clone().sub(@node2.position)

## Undirected graph
class window.Graph
  nodeSet: {}
  nodes: []
  edges: []

  addNode: (id, data = {}) ->
    if @nodeSet[id]? then throw "node #{id} already in graph"
    node = new Node(id, data)
    @nodeSet[id] = node
    @nodes.push(node)
    node
  node: (id) ->
    r = @nodeSet[id]
    if not r? then throw "node #{id} not in graph"
    r

  addEdge: (from, to, weight, data = {}) ->
    if not @nodeSet[from.id] then throw "node #{from.id} not in graph"
    if not @nodeSet[to.id] then throw "node #{to.id} not in graph"
    edge = new Edge(from, to, weight, data)
    @edges.push(edge)
    from.addEdge(edge)
    to.addEdge(edge)
    edge

epsilon = 0.01
epsilonSq = epsilon * epsilon
epsilonVector = new Vector(epsilon, 0, 0)

class EdgeSpringLayout
  constructor: (@edge, spring) ->
    @spring = Math.max(epsilon, Math.min(0.95, @edge.weight * spring))
  # Hooke's law, edges act like springs
  attract: ->
    f = @edge.vector().clone().multiplyScalar(@spring)
    @edge.node2.layout.applyForce(f)
    @edge.node1.layout.applyForce(f.negate())
    f

class NodeSpringLayout
  constructor: (@node, @repulsionConstant) ->
    @force = new Vector()
  applyForce: (forceVector) -> @force.add(forceVector)
  # Coulomb's law repulses the nodes.
  #  Apply our repulsion to all other nodes
  repulse: (nodes, offset) ->
    for i in [offset..nodes.length-1]
      other = nodes[i]
      f = other.position.clone().sub(@node.position)
      distanceSq = f.lengthSq()
      if (distanceSq < epsilonSq)
        f = epsilonVector
        distanceSq = epsilonSq
      f.setLength(@repulsionConstant / distanceSq)
      other.layout.applyForce(f)
      @applyForce(f.negate())
    this
  moveAccordingToForce: ->
    t = @force.length()
    @node.position.add(@force)
    @force.set(0,0,0)
    t

## Layouting algorithm based on repulsion between nodes and edges acting like springs.
class window.SpringLayout
  constructor: (@graph, @size, repulsion = 1, spring = 1/6) ->
    density = Math.pow(@size.x*@size.y*@size.z / @graph.nodes.length, 1/3)
    repulsionConstant = Math.pow(repulsion * density, 2)
    node.layout = new NodeSpringLayout(node, repulsionConstant) for node in @graph.nodes
    maxWeight = Math.max((e.weight for e in @graph.edges)...)
    springValue = spring / maxWeight
    edge.layout = new EdgeSpringLayout(edge, springValue) for edge in @graph.edges
    @temperature = 1000000

  step: ->
    @graph.nodes[i].layout.repulse(@graph.nodes, i+1) for i in [0..@graph.nodes.length-2]
    edge.layout.attract() for edge in @graph.edges
    movements = 0
    movements += node.layout.moveAccordingToForce() for node in @graph.nodes
    @temperature = 1000000 * movements / Math.pow(@graph.nodes.length, 2)
    @temperature


## SpringLayout with clustered repulsion (faster).
class window.ClusterSpringLayout
  constructor: (@graph, @size, repulsion = 1, spring = 1/6, @clusterCount = 7) ->
    density = Math.pow(@size.x*@size.y*@size.z / @graph.nodes.length, 1/3)
    @repulsionConstant = Math.pow(repulsion * density, 2)
    node.layout = new NodeClusterSpringLayout(node, @repulsionConstant) for node in @graph.nodes
    maxWeight = Math.max((e.weight for e in @graph.edges)...)
    springValue = spring / maxWeight
    edge.layout = new EdgeSpringLayout(edge, springValue) for edge in @graph.edges
    @temperature = 1000000

  # Cluster the nodes into count^3 clusters
  mkClusters: (count) ->
    min = @graph.nodes[0].position.clone()
    max = min.clone()
    for node in @graph.nodes
      min = min.min(node.position)
      max = max.max(node.position)
    sizePart = max.sub(min).divideScalar(count)
    clusters = []
    maxIndex = count * count * count - 1
    for node in @graph.nodes
      x = Math.floor((node.position.x - min.x) / sizePart.x)
      y = Math.floor((node.position.y - min.y) / sizePart.y)
      z = Math.floor((node.position.z - min.z) / sizePart.z)
      i = Math.min(x*count*count + y*count + z, maxIndex)
      cluster = clusters[i]
      if not cluster?
        clusters[i] = {
          x: x
          y: y
          z: z
          centerOfMass: new Vector(min.x+sizePart.x*(x+0.5), min.y+sizePart.y*(y+0.5), min.z+sizePart.z*(z+0.5))
          mass: 1
          nodes: [node]
        }
      else
        cluster.nodes.push(node)
        cluster.mass++
    clusters
  
  step: ->
    clusters = @mkClusters(@clusterCount)

    for clusterA, i in clusters when clusterA?
      for j in [i..clusters.length-1]
        clusterB = clusters[j]
        if clusterB?
          dx = clusterA.x-clusterB.x; dy = clusterA.y-clusterB.y; dz = clusterA.z-clusterB.z
          if dx>=-1 and dx<=1 and dy>=-1 and dy<=1 and dz>=-1 and dz<=1
            node.layout.repulseNodes(clusterB.nodes) for node in clusterA.nodes
          else
            f = clusterA.centerOfMass.clone().sub(clusterB.centerOfMass)
            distanceSq = f.lengthSq()
            f.setLength(clusterB.mass * @repulsionConstant / distanceSq)
            node.layout.force.add(f) for node in clusterA.nodes
            f.setLength(clusterA.mass * @repulsionConstant / distanceSq)
            node.layout.force.sub(f) for node in clusterB.nodes

    edge.layout.attract() for edge in @graph.edges
    movements = 0
    movements += node.layout.moveAccordingToForce() for node in @graph.nodes
    @temperature = 1000000 * movements / Math.pow(@graph.nodes.length, 2)
    @temperature

class NodeClusterSpringLayout
  constructor: (@node, @repulsionConstant) ->
    @force = new Vector()
  applyForce: (forceVector) -> @force.add(forceVector)

  repulseNodes: (nodes) ->
    for other in nodes when @node != other
      f = @node.position.clone().sub(other.position)
      distanceSq = f.lengthSq()
      if (distanceSq < epsilonSq)
        f = epsilonVector
        distanceSq = epsilonSq
      f.setLength(@repulsionConstant / distanceSq)
      @force.add(f)
      other.layout.force.sub(f)

  moveAccordingToForce: ->
    t = @force.length()
    @node.position.add(@force)
    @force.set(0,0,0)
    t



time = -> new Date().getTime()

## Applies an incremental layout (with .step() and .temperature()).
class window.Layouter
  class Stats
    constructor: ->
      @time = 0
      @iterations = 0
    add: (t, i) ->
      @time += t
      @iterations += i
    reset: ->
      @time = 0
      @iterations = 0
    i_per_ms: -> @iterations / @time
    ms_per_i: -> @time / @iterations
  constructor: (@graph, @temperatureLimit, @debugInterval = 3000) ->
    @done = @graph.temperature < @temperatureLimit
    @statsTotal = new Stats()
    @statsRound = new Stats()
    @statsRound.lastPrinted = time()

  step: (durationMs) -> if not @done
    t0 = time()
    iterations = 0
    while time()-t0 < durationMs and not @done
      @graph.layout.step()
      @done = @graph.layout.temperature < @temperatureLimit
      iterations++
    #update stats
    t1 = time()
    @statsTotal.add(t1 - t0, iterations)
    @statsRound.add(t1 - t0, iterations)
    if @done
      console.debug("Done with graph layouting after #{@statsTotal.iterations} iterations. Duration: #{(@statsTotal.time/1000).toFixed(1)}s; Performance: #{@statsTotal.ms_per_i().toFixed(3)}ms/it")
    if t1-@statsRound.lastPrinted > @debugInterval
      console.debug("Graph layouting: temp=#{@graph.layout.temperature.toFixed(0)} after #{@statsTotal.iterations} iterations. Current: #{@statsRound.ms_per_i().toFixed(3)}ms/it; Total: #{@statsTotal.ms_per_i().toFixed(3)}ms/it")
      @statsRound.reset()
      @statsRound.lastPrinted = t1
    not @done
