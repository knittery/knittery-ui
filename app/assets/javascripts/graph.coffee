Vector = THREE.Vector3

class Node
  constructor: (@id, @data) ->

  position: new Vector()
  neighbors: {}
  edges: []

  addEdge: (edge) ->
    @edges.push(edge)
    n = edge.other(this)
    @neighbors[n.id] = n



class Edge
  constructor: (@node1, @node2, @weight) ->
  other: (node) ->
    if @node1 == node then @node2
    else if @node2 == node then @node1
    else throw "edge #{@node1.id}-#{@node2.id} does not contain #{node.id}"
  nodes: -> [@node1, @node2]
  vector: -> @node1.position.clone().sub(@node2.position)


class Graph
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

  addEdge: (from, to, weight) ->
    if not @nodeSet[from.id] then throw "node #{from.id} not in graph"
    if not @nodeSet[to.id] then throw "node #{to.id} not in graph"
    edge = new Edge(from, to, weight)
    @edges.push(edge)
    from.addEdge(edge)
    to.addEdge(edge)
    edge

window.Graph = Graph

epsilon = 0.000001
epsilonSq = epsilon * epsilon
epsilonVector = new Vector(0.000001, 0, 0)

class SpringLayout
  constructor: (@graph, @size, repulsion = 1, spring = 2) ->
    density = Math.pow(@size.x*@size.y*@size.z / @graph.nodes.length, 1/3)
    repulsionConstant = Math.pow(repulsion * density, 2)
    node.layout = new NodeLayout(node, repulsionConstant) for node in @graph.nodes
    springValue = spring / @graph.nodes.length
    edge.layout = new EdgeLayout(edge, springValue) for edge in @graph.edges

  class NodeLayout
    constructor: (@node, @repulsionConstant) ->
      @force = new Vector()
    applyForce: (forceVector) -> @force.add(forceVector)
    # Coulomb's law repulses the nodes.
    #  Apply our repulsion to all other nodes
    repulse: (nodes) ->
      for other in nodes when other != @node
        f = @node.position.clone().sub(other.position).negate()
        distanceSq = f.lengthSq()
        if (distanceSq < epsilon)
          f = epsilonVector
          distanceSq = epsilonSq
        f.normalize().multiplyScalar(@repulsionConstant / distanceSq)
        other.layout.applyForce(f)
      this
    moveAccordingToForce: ->
      @node.position.add(@force)
      @force.set(0,0,0)
      this



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
    node.layout.repulse(@graph.nodes)  for node in @graph.nodes
    edge.layout.attract()              for edge in @graph.edges
    node.layout.moveAccordingToForce() for node in @graph.nodes
    this

window.SpringLayout = SpringLayout