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

def day06(input, part)
  state = input.split("\t").map(&:to_i)
  seen = {}
  loop.with_index do |_, t|
    return part == :b ? t - seen[state] : t if seen[state]
    seen[state] = t
    max_val = state.max
    max_ix = state.find_index max_val
    state = state.map.with_index{|e, i| (max_ix == i ? 0 : e) + max_val / state.size +
                                        ((i - max_ix - 1) % state.size < max_val % state.size ? 1 : 0)}
  end
end

day06in = "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"

time = Time.now
p day06(day06in, :b)
puts "time: #{Time.now - time}"
