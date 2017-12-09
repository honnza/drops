def day01(input, part)
  input.tr! "^0-9", ""
  pairs = case part
  when :a then (input + input[0]).chars.each_cons(2)
  when :b then input[0 ... input.size/2].chars.zip(input[input.size/2 .. -1].chars) * 2
  end
  pairs.select{|a, b| a==b}.map{|a, b| a.to_i}.reduce(0, &:+)
end

def day02(input, part)
  rows = input.lines.map{|r| r.split(" ").map(&:to_i)}
  case part
  when :a then rows.map{|r| r.max - r.min}.reduce(0, &:+)
  when :b then rows.map{|r| x = nil; r.find{|n| r.find{|d| x = n / d if n != d && n % d == 0}}; x}.reduce(0, &:+)
  end
end

def day03(input, part)
  case part
  when :a
    major_coord = (input ** 0.5 * 0.5 - 0.5).ceil
    minor_coord = ((2 * major_coord + 1) ** 2 - input) % (2 * major_coord)
    minor_coord = (major_coord - minor_coord).abs
    
    minor_coord + major_coord
  when :b
    board = Hash.new(0); board[[0, 0]] = 1; x = 0; y = 0
    loop.with_index(1) do
      p [x, y, board[[x, y]]]; gets
      return board[[x, y]] if board[[x, y]] > input
      
      case
      when x == y && x < 0 then y += 1
      when x.abs == y.abs then x += y <=> -0.5
      when x.abs < y.abs then x += y <=> 0
      else y += 0 <=> x
      end

      board[[x, y]] = [[0, 1], [0, -1], [1, 0], [-1, 0], [-1, -1], [-1, 1], [1, -1], [1, 1]]
          .map{|dx, dy| board[[x+dx, y+dy]]}.reduce(&:+)
    end
  end
end

def day04(input, part)
  input.lines.count do |line|
    ws = line.split(" ")
    ws.map!{|w| w.chars.sort.join} if part == :b
    ws == ws.uniq
  end
end

def day05(input, part)
  maze = input.lines.map(&:to_i)
  maze_ix = 0
  loop.with_index(1) do |_, t|
    old_ix = maze_ix
    maze_ix += maze[maze_ix]
    maze[old_ix] += part == :b && maze[old_ix] >= 3 ? -1 : 1
    return t if maze_ix < 0 || maze_ix >= maze.size
  end
end

day06in = "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"

def day06(input, part)
  state = input.split("\t").map(&:to_i)
  seen = {}
  loop.with_index do |_, t|
    return  t - seen[state], t if seen[state]
    seen[state] = t
    max_val = state.max
    max_ix = state.find_index max_val
    state = state.map.with_index{|e, i| (max_ix == i ? 0 : e) + max_val / state.size +
                                        ((i - max_ix - 1) % state.size < max_val % state.size ? 1 : 0)}
  end
end

Day07Node = Struct.new :name, :weight, :child_names, :children
def day07(input, part)
  nodes = input.lines.map do |line|
    line =~ /([a-z]+) \((\d+)\)(?: -> ([a-z, ]+))?/
    Day07Node.new $1, $2.to_i, $3 ? $3.split(", ") : [], nil
  end
  
  root_candidates = nodes.dup
  nodes_by_name = Hash[nodes.map{|node| [node.name, node]}]
  nodes.each{|node| node.children = node.child_names.map{|name| nodes_by_name[name]}}
  nodes.each{|node| node.children.each{|c| root_candidates.delete c}}
  
  root = root_candidates[0]
  return root_name if part == :a  
  
  subtree_weight_or_return = Proc.new do |node|
    weights = node.children.map(&subtree_weight_or_return)
    if weights.uniq.size <= 1
      weights.reduce(0, &:+) + node.weight
    else
      wrong_weight_at = weights.find_index{|w| weights.count(w) == 1}
      wrong_weight = weights[wrong_weight_at]
      correct_weight = weights[wrong_weight_at == 0 ? 1 : 0]
      return correct_weight - wrong_weight + node.children[wrong_weight_at].weight
    end
  end
  subtree_weight_or_return[root]
end

def day08(input, part)
  regs = Hash.new(0)
  max = 0
  input.lines.each do |line|
    line =~ /([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (>|>=|==|!=|<|<=) (-?\d+)/
    regs[$1] += $3.to_i * ($2 == "inc" ? 1 : -1) if regs[$4].send($5, $6.to_i)
    max = [max, regs.values.max].max
  end
  [regs.values.max, max]
end

class ParseError < StandardError; end
Day09Stream = Struct.new :elems, :score, :score_total, :garbage_removed do
    def self.parse_from(io, score)
      elems = []
      await_comma = false
      loop do
        case (char = io.read(1))
        when "{"
          raise ParseError if await_comma
          elems << Day09Stream.parse_from(io, score + 1)
          await_comma = true
        when "<" 
          raise ParseError if await_comma
          elems << Day09Garbage.parse_from(io)
          await_comma = true
        when ","
          raise ParseError unless await_comma
          await_comma = false
        when "}" 
          return new(elems, score, score + elems.map(&:score_total).reduce(0, &:+),
                     elems.map(&:garbage_removed).reduce(0, &:+))
        else raise ParseError
        end
      end
    end
end
Day09Garbage = Struct.new :garbage_removed do
  def score_total; 0; end
  def self.parse_from(io)
    garbage_removed = 0
    loop do
      case (char = io.read(1))
      when ">" then return new(garbage_removed)
      when "!" then io.read(1)
      else garbage_removed += 1
      end
    end
  end
end
def day09(input, part)
  io = StringIO.new(input)
  raise ParseError unless io.read(1) == "{"
  stream = Day09Stream.parse_from(io, 1)
  return stream.score_total, stream.garbage_removed
end

time = Time.now
p day09(File.read("day09in.txt"), :b)
puts "time: #{Time.now - time}"
