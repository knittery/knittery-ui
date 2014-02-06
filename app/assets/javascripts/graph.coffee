class Node
  constructor: (@id, @data) ->

  position: {}
  neighbors: {}
  edges: []

  addEdge: (edge) ->
    @edges.push(edge)
    n = edge.other(this)
    @neighbors[n.id] = n



class Edge
  constructor: (@node1, @node2) ->
  other: (node) ->
    if @node1 == node then @node2
    else if @node2 == node then @node1
    else throw "edge #{@node1.id}-#{@node2.id} does not contain #{node.id}"
  nodes: -> [@node1, @node2]


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

  addEdge: (from, to) ->
    if not @nodeSet[from.id] then throw "node #{from.id} not in graph"
    if not @nodeSet[to.id] then throw "node #{to.id} not in graph"
    edge = new Edge(from, to)
    @edges.push(edge)
    from.addEdge(edge)
    to.addEdge(edge)
    edge

window.Graph = Graph