def next_move(box, i)
  is_n = i <= 2
  is_e = i % 3 == 2
  is_s = i >= 6
  is_w = i % 3 == 0
  $neighbors_4 ||= []
  $neighbors_4[i] ||= [
    (i - 3 unless is_n),
    (i + 1 unless is_e),
    (i + 3 unless is_s),
    (i - 1 unless is_w)
  ].compact
  $neighbors_8 ||= []
  $neighbors_8[i] ||= [
    (i - 4 unless is_n || is_w),
    (i - 3 unless is_n),
    (i - 2 unless is_n || is_e),
    (i + 1 unless is_e),
    (i + 4 unless is_s || is_e),
    (i + 3 unless is_s),
    (i + 2 unless is_s || is_w),
    (i - 1 unless is_w)
  ].compact

  effect = box[i] == "B" ? box[4] : box[i]
  new_box = box.dup
  case effect
  when "A", "B" then nil
  when "G" 
    new_box[i] = box[8 - i]
    new_box[8 - i] = box[i]
  when "K"
    row = i - i % 3
    new_box[row, 3] = box[row + 2] + box[row, 2]
  when "O"
    tally = $neighbors_4[i].map{box[_1]}.tally
    max = tally.values.max
    return if tally.values.count(max) > 1
    new_box[i] = tally.invert[max]
  when "P"
    $new_neighbors ||= []
    $new_neighbors[i] ||= $neighbors_8[i][1 ...] + $neighbors_8[i][... 1]
    $neighbors_8[i].each_index{new_box[$new_neighbors[i][_1]] = box[$neighbors_8[i][_1]]}
  when "R"
    new_box.tr!("K", box[i])
    new_box.tr!("W", "K")
  when "V"
    return if is_s
    new_box[i] = box[i + 3]
    new_box[i + 3] = box[i]
  when "W"
    new_box[i] = "A"
    $neighbors_4[i].each{new_box[_1] = box[_1].tr(box[i] + "A", "A" + box[i])}
  when "Y"
    return if is_n
    new_box[i] = box[i - 3]
    new_box[i - 3] = box[i]
  else raise "unknown color #{box[i]}"
  end
  new_box unless new_box == box
end

begin
  raise "expected exactly one argument" unless ARGV.count == 1
  unless %r{^(?<goal>([ABGKOPRVWY]|[ABGKOPRVWY]{4}))(?<start>(?:/[ABGKOPRVWY]{3}){3})$} =~ ARGV[0]
    raise "expected format: goal/top row/middle row/bottom row, all left to right"
  end
rescue
  puts $!
  exit
end
goal *= 4 if goal.length == 1

PLTE = {?A => [127, 127, 127], ?B => [0, 127, 255],   ?G => [0, 255, 0],
        ?O => [255, 128, 0],   ?P => [255, 127, 255], ?R => [255, 0, 0],
        ?V => [127, 0, 255],   ?W => [255, 255, 255], ?Y => [255, 255, 0],
        ?K => [63, 63, 63]}
def fancy_box(box, i)
  [0, 3, 6].map do |row|
    [0, 1, 2].map do |col|
      tile = "\e[48;2;%d;%d;%dm  \e[0m" % PLTE[box[row + col]]
      row + col == i ? "[#{tile}]" : " #{tile} "
    end.join
  end.join("\n\n")
end

start.tr!("/", "")

prev_nodes = {start => []}
reachable = [start]
reachable.each.with_index do |from, t|
  puts "%d/%d %2.2f%%" % [t, reachable.count, t.to_f / reachable.count * 100] if t % 4096 == 0 && t > 0
  (0..8).each do |i|
    to = next_move(from, i)
    next if to.nil?
    unless prev_nodes[to]
      reachable << to unless to.nil?
      prev_nodes[to] = []
    end
    prev_nodes[to] << from
  end
end
puts "reachable states: #{reachable.count}"

solve_cost = {}
solvable = []
reachable.each do |box|
  if box[0] == goal[0] && box[2] == goal[1] && box[6] == goal[2] && box[8] == goal[3]
    solve_cost[box] = 0
    solvable << box
  end
end
solvable.each do |to|
  prev_nodes[to].each do |from|
    unless solve_cost[from]
      solve_cost[from] = solve_cost[to] + 1
      solvable << from
    end
  end
end

puts "solvable states: #{solvable.count}"
exit if solvable.empty?
puts "solution length: #{solve_cost[start]}"

node = start
loop do
  puts node.scan(/.../).join("/")
  if solve_cost[node] == 0
    puts fancy_box(node, nil)
    break
  else
    i = (0 .. 8).find{|i| solve_cost[next_move(node, i)] == solve_cost[node] - 1}
    puts fancy_box(node, i)
    node = next_move(node, i)
  end
end
