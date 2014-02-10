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

epsilon = 0.000001
epsilonSq = epsilon * epsilon
epsilonVector = new Vector(0.000001, 0, 0)

## Layouting algorithm based on repulsion between nodes and edges acting like springs.
class window.SpringLayout
  constructor: (@graph, @size, repulsion = 1, spring = 1/6) ->
    density = Math.pow(@size.x*@size.y*@size.z / @graph.nodes.length, 1/3)
    repulsionConstant = Math.pow(repulsion * density, 2)
    node.layout = new NodeLayout(node, repulsionConstant) for node in @graph.nodes
    maxWeight = Math.max((e.weight for e in @graph.edges)...)
    springValue = spring / maxWeight
    edge.layout = new EdgeLayout(edge, springValue) for edge in @graph.edges
    @temperature = 1000000

  class NodeLayout
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

  class EdgeLayout
    constructor: (@edge, spring) ->
      @spring = Math.max(epsilon, Math.min(0.95, @edge.weight * spring))
    # Hooke's law, edges act like springs
    attract: ->
      f = @edge.vector().clone().multiplyScalar(@spring)
      @edge.node2.layout.applyForce(f)
      @edge.node1.layout.applyForce(f.negate())
      f

  step: ->
    @graph.nodes[i].layout.repulse(@graph.nodes, i+1) for i in [0..@graph.nodes.length-2]
    edge.layout.attract() for edge in @graph.edges
    movements = 0
    movements += node.layout.moveAccordingToForce() for node in @graph.nodes
    @temperature = 1000000 * movements / Math.pow(@graph.nodes.length, 2)
    @temperature

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
