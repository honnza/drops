Node = Array
Edge = Struct.new :nodes, :weight

Neigborhoods = {
  :cubic => [[ 0 , 0, -1],
             [ 0,  0,  1],
             [ 0, -1,  0],
             [ 0,  1,  0],
             [-1,  0,  0],
             [ 1,  0,  0]],

  :hexagonal => [[ 0,  1, -1],
                 [ 1,  0, -1],
                 [ 1, -1,  0],
                 [ 0, -1,  1],
                 [-1,  0,  1],
                 [-1,  1,  0]]
}

Neigborhood = Neigborhoods[:cubic]

def neighbors coords
  Neigborhood.map {|d| coords.zip(d).map{|c, d| (c + d)}}
end

def priority c1, c2
  c1[2] == c2[2] ? 2 : 1
end

nodes = {[0, 0, 1] => Node.new}
edges = neighbors([0, 0, 1]).map{|c| Edge.new [[0, 0, 1], c], rand * priority([0, 0, 1], c)}.sort_by(&:weight)

until edges.empty?
  edge_in = edges.pop
  from, to = edge_in.nodes
  next if nodes[from].size >= 3
  next if nodes[to]
  next if to[2] < 1
  next if edge_in.nodes.last.any? {|c| c.abs > 3}

  nodes[edge_in.nodes.last] = Node.new
  edge_in.nodes.each{|n| nodes[n] << edge_in}
  edges += neighbors(edge_in.nodes.last).map{|c| Edge.new [to, c], rand * priority(to, c) }
  edges.sort_by!(&:weight)

  p edge_in
  gets
end
