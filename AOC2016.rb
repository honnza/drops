require'digest'
require'set'

def aoc1a input
  x, y = 0, 0
  input.scan /([LR])(\d+)/ do
    x, y = $1 == ?L ? [-y, x] : [y, -x]
    y += $2.to_i
  end
  x.abs + y.abs
end

def aoc1b input
  trace = []
  x, y = 0, 0
  input.scan /([LR])(\d+)/ do
    x, y = $1 == ?L ? [-y, x] : [y, -x]
    trace.map! { |x, y| $1 == ?L ? [-y, x] : [y, -x]}
    $2.to_i.times do
      y += 1
      return x.abs + y.abs if trace.include? [x, y]
      trace << [x, y]
    end
  end
end

def aoc2a input
  x, y = -2, 0
  input.lines.map do |line|
    line.each_char do |char|
      case char
      when ?L then x -= 1 unless x == -1
      when ?R then x += 1 unless x ==  1
      when ?U then y -= 1 unless y == -1
      when ?D then y += 1 unless y ==  1
      end
    end
    x + 3 * y + 5
  end.join
end

def aoc2b input
  x, y = 0, 0
  keypad = [
    [nil, nil, "1", nil, nil],
    [nil, "2", "3", "4", nil],
    ["5", "6", "7", "8", "9"],
    [nil, "A", "B", "C", nil],
    [nil, nil, "D", nil, nil]
  ]
  input.lines.map do |line|
    line.each_char do |char|
      case char
      when ?L then x -= 1 unless x - y.abs == -2
      when ?R then x += 1 unless x + y.abs ==  2
      when ?U then y -= 1 unless y - x.abs == -2
      when ?D then y += 1 unless y + x.abs ==  2
      end
    end
    keypad[y + 2][x + 2]
  end.join
end

def aoc3a input
  input.lines.select do |line|
    a, b, c = line.scan(/\d+/).map(&:to_i).sort
    a + b > c
  end.count
end

def aoc3b input
  tbl = input.lines.map{|l| l.scan(/\d+/).map(&:to_i)}
  tbl = tbl.each_slice(3).map(&:transpose).flatten(1)
  tbl.select do |line|
    a, b, c = line.sort
    a + b > c
  end.count
end

def aoc4a input
  input.lines.map do |line|
    line =~ /(.+?)-(\d+)\[(.+?)\]/
    $2.to_i if $3 == [*?a..?z].sort_by.with_index{|c, i| [-$1.count(c), i]}[0..4].join
  end.compact.reduce(0, &:+);
end

def aoc4b input
  alpha = [*?a .. ?z].join
  input.lines.map do |line|
    line =~ /(.+?)-(\d+)\[(.+?)\]/
    if $3 == [*?a..?z].sort_by.with_index{|c, i| [-$1.count(c), i]}[0..4].join
      shift = $2.to_i % 26
      key = alpha[shift .. -1] + alpha[0 ... shift]
      line.tr alpha, key
    end
  end.compact.sort;
end

def aoc5a input
  (0 .. Float::INFINITY).lazy.map do |i|
    digest = Digest::MD5.hexdigest(input + i.to_s)
    digest[5] if digest[0..4] == "00000"
  end.select(&:itself).take(8).to_a.join;
end

def aoc5b input
  result = Array.new(8, nil)
  loop.with_index do |_, i|
    digest = Digest::MD5.hexdigest(input + i.to_s)
    ri = digest[5].to_i(16)
    if digest[0..4] == "00000" && ri < 8 && result[ri].nil?
      result[ri] = digest[6]
      puts result.map{|e|e.to_s[0]}.join
      return result.join unless result.any? &:nil?
    end
  end
end

def aoc6a input
  input.lines.map{|l| l.chomp.chars}.transpose.map do |col|
    col.uniq.max_by{|c|col.count c}
  end.join
end

def aoc6b input
  input.lines.map{|l| l.chomp.chars}.transpose.map do |col|
    col.uniq.min_by{|c|col.count c}
  end.join
end

def aoc7a input
  abba = /(.)(?!\1)(.)\2\1/
  input.lines.select do |line|
    case line
    when /\[[^\]]*#{abba}/ then false
    when abba then true
    else false
    end
  end.count
end

def aoc7b input
  input.lines.select do |line|
    supernets = line.gsub /\[.+?\]/, "||"
    hypernets = line.scan(/\[.+?\]/).join.tr("[]", "|")
    supernets.scan(/(?=(.)(?!\1)(.)\1)/).any? do |a, b|
      hypernets.include? "#{b}#{a}#{b}"
    end
  end.count
end

def aoc8ab input
  lcd = Array.new(6){["."] * 50}
  input.each_line do |line|
    case line
    when /rect (\d+)x(\d+)/
      lcd[0 ... $2.to_i].each{|row| row.fill("#", 0 ... $1.to_i) }
    when /rotate row y=(\d+) by (\d+)/
      lcd[$1.to_i].tap{|row| row.unshift *row.pop($2.to_i)}
    when /rotate column x=(\d+) by (\d+)/
      lcd = lcd.transpose
      lcd[$1.to_i].tap{|col| col.unshift *col.pop($2.to_i)}
      lcd = lcd.transpose
    end

    puts line
    puts lcd.map(&:join).join(?\n)
    sleep 1
  end
  lcd.join.count "#"
end

def aoc9a input
  buf = input.dup
  offset = 0
  loop do
    match = buf.match /\((\d+)x(\d+)\)/, offset
    return buf.length unless match
    rep_str = buf[match.offset(0).last, match[1].to_i]
    buf[match.offset(0).last, match[1].to_i] = rep_str * match[2].to_i
    buf[match.offset(0).first ... match.offset(0).last] = ""
    offset = match.offset(0).first + rep_str.length * match[2].to_i
  end
end

def aoc9b input
  offset = 0
  r = 0
  loop do
    match = input.match /\((\d+)x(\d+)\)/, offset
    return r + input.length - offset unless match
    r += match.offset(0).first - offset
    r += aoc9b(input[match.offset(0).last, match[1].to_i]) * match[2].to_i
    offset = match.offset(0).last + match[1].to_i
  end
end

def aoc10ab input
  input2 = [17, 61]
  bots = Hash.new{|h, k| h[k] = {id: k, outs: nil, values: []}}
  outputs = {}

  input.each_line do |line|
    case line.chomp
    when /value (\d+) goes to bot (\d+)/
      bots[$2.to_i][:values] << $1.to_i
    when /bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)/
      bots[$1.to_i][:outs] = [[$2, $3.to_i], [$4, $5.to_i]]
    end
  end

  eval_queue = bots.values.select{|bot| bot[:values].count == 2}
  until eval_queue.empty?
    bot = eval_queue.pop
    puts "part a: #{bot[:id]}" if bot[:values].sort == input2
    bot[:outs].zip bot[:values].sort do |out, value|
      if out[0] == "output"
        outputs[out[1]] = value
      else
        bots[out[1]][:values] << value
        eval_queue << bots[out[1]] if bots[out[1]][:values].count == 2
      end
    end
  end

  puts "part b: #{outputs[0] * outputs[1] * outputs[2]}"
end

def aoc11ab part, input
  materials = input.scan(/(\w+) generator/).map(&:first)
  floors = input.lines.map do |line|
    floor = {first: 0, second: 1, third: 2, fourth: 3}[line[/(\w+) floor/, 1].to_sym]
    line.scan(/(\w+)( generator|-compatible microchip)/).map{|mat, type| [mat, type[/ ./][1]]}
  end
  init_state = [0, Array.new(4){[0, 0, 0, 0]}]
  materials.each do |mat|
    gen_floor = floors.index{|f| f.include? [mat, "g"]}
    mic_floor = floors.index{|f| f.include? [mat, "m"]}
    init_state[1][gen_floor][mic_floor] += 1
  end
  if part == :b
    materials += ["elerium", "dilithium"]
    init_state[1][0][0] += 2
  end

  ix_transitions = Hash.new { |h, k| h[k] = [] }
  (0..3).each do |gen|
    (0..3).each do |mic|
      ix_transitions[[gen, gen - 1]] << [gen, mic, gen - 1, mic] if gen > 0
      ix_transitions[[gen, gen + 1]] << [gen, mic, gen + 1, mic] if gen < 3
      ix_transitions[[mic, mic - 1]] << [gen, mic, gen, mic - 1] if mic > 0
      ix_transitions[[mic, mic + 1]] << [gen, mic, gen, mic + 1] if mic < 3
    end
  end

  valid_state = lambda do |state|
    (0..3).all? do |floor|
      has_gen = state[1][floor].any?{|pc| pc > 0}
      has_mic = ([*0..3]-[floor]).any?{|gen_floor| state[1][gen_floor][floor] > 0}
      !(has_gen && has_mic) && state[1][floor].all?{|pc| pc >= 0}
    end
  end
  final_state = ->state{state[1][3][3] == materials.count}
  next_states = lambda do |state|
    elevator, pairs = state
    [(-1 if elevator > 0), (1 if elevator < 3)].compact.map do |dir|
      valid_transitions = ix_transitions[[elevator, elevator + dir]]
      r = (valid_transitions + [nil]).repeated_combination(2).map do |t1, t2| #heterogeneous moves and single moves
        if t1 || t2
          new_pairs = pairs.map(&:dup)
          new_pairs[t1[0]][t1[1]] -= 1 if t1
          new_pairs[t1[2]][t1[3]] += 1 if t1
          new_pairs[t2[0]][t2[1]] -= 1 if t2
          new_pairs[t2[2]][t2[3]] += 1 if t2
          [elevator + dir, new_pairs]
        end
      end

      r << [elevator + dir, pairs.map(&:dup)] #homogeneous move
      r.last[1][elevator][elevator] -= 1
      r.last[1][elevator + dir][elevator + dir] += 1

      r.compact.select(&valid_state)
    end.flatten(1)
  end

  open_set = {init_state => {s: init_state, g: 0}}
  closed_set = Set.new
  loop do
    node = open_set.first[1]
    open_set.delete node[:s]
    closed_set << node[:s]
    next_states[node[:s]].each do |next_state|
      prev_next = open_set[next_state]
      unless closed_set.include? next_state || (prev_next && prev_next[:g] <= node[:g] + 1)
        open_set.delete prev_next[:s] if prev_next
        next_node = {s: next_state, g: node[:g] + 1}
        return next_node[:g] if final_state[next_state]
        open_set[next_state] = next_node
      end
    end
  end
end

def aoc12ab part, input
  code = input.lines
  regs = {?a => 0, ?b => 0, ?c => (part == :b ? 1 : 0), ?d => 0}
  ip = 0
  while ip < code.length
    case code[ip]
    when /^cpy ([-\d]+) (.)$/ then regs[$2] = $1.to_i
    when /^cpy (.) (.)$/ then regs[$2] = regs[$1]
    when /^inc (.)$/ then regs[$1] += 1
    when /^dec (.)$/ then regs[$1] -= 1
    when /^jnz (.) ([-\d]+)$/ then ip += $2.to_i - 1 if regs[$1] != 0
    end
    ip += 1
  end
  regs[?a]
end

def aoc13a
  tgt = [31, 39]
  wall_hsh = lambda do |node|
    x, y = node
    n = x*x + 3*x + 2*x*y + y + y*y + 1358
    n.to_s(2).count(?1).odd? || x < 0 || y < 0
  end

  prev_lvl = nil
  this_lvl = Set[]
  next_lvl = Set[[1, 1]]
  loop.with_index(1) do |_, depth|
    prev_lvl = this_lvl
    this_lvl = next_lvl
    next_lvl = Set[]
    this_lvl.each do |x, y|
      [[x, y+1], [x, y-1], [x+1, y], [x-1, y]].each do |next_node|
        return depth if next_node == tgt
        next_lvl << next_node unless prev_lvl.include?(next_node) || wall_hsh[next_node]
      end
    end
  end
end

def aoc13b
  tgt = 50
  wall_hsh = lambda do |node|
    x, y = node
    n = x*x + 3*x + 2*x*y + y + y*y + 1358
    n.to_s(2).count(?1).odd? || x < 0 || y < 0
  end

  prev_lvls = Set[]
  this_lvl = Set[]
  next_lvl = Set[[1, 1]]
  tgt.times do
    prev_lvls += this_lvl
    this_lvl = next_lvl
    next_lvl = Set[]
    this_lvl.each do |x, y|
      [[x, y+1], [x, y-1], [x+1, y], [x-1, y]].each do |next_node|
        next_lvl << next_node unless prev_lvls.include?(next_node) || wall_hsh[next_node]
      end
    end
  end
  (prev_lvls + this_lvl + next_lvl).count
end

def aoc14ab part
  calc_hash = case part
  when :a then ->str{Digest::MD5.hexdigest str}
  when :b then ->str{2017.times{str = Digest::MD5.hexdigest str}; str}
  end

  input = "ahsbgdzn"
  window = Array.new(1001){|i| calc_hash[input + i.to_s]}
  keysGot = 0

  loop.with_index(1001) do |_, i|
    match = window[0].match /(.)\1\1/
    if match && window[1..-1].any?{|hsh| hsh.include?(match[1] * 5)}
      keysGot += 1
      puts "%s found at %d (%d/64)" % [window[0], i - 1001, keysGot]
      return (i - 1001) if keysGot == 64
    end
    window.shift
    window << calc_hash[input + i.to_s]
  end
end

def aoc15ab part, input
  nfm = -> a, b {(a % b + b) % b} # negative-friendly modulo - like Ruby's %, except it always returns non-negative values
  discs = input.scan(/Disc #(\d)+ has (\d+) positions; at time=0, it is at position (\d+)\./).map{|d|d.map(&:to_i)}
  discs << [discs.count + 1, 11, 0]
  discs.reduce([1, 0]) do |a, d|
    b = [d[1], nfm[-(d[0] + d[2]), d[1]]]
    a, b = b, a if a[0] < b[0]
    eea_prev = [a[0], 0, 1]
    eea_last = [b[0], 1, 0]
    until eea_last[0] == 1
      q = eea_prev[0] / eea_last[0]
      eea_prev, eea_last = eea_last, eea_last.zip(eea_prev).map{|last, prev| prev - q * last}
    end

    mod = a[0] * b[0]
    val = nfm[a[0] * eea_last[2] * b[1] + b[0] * eea_last[1] * a[1], mod]
    raise "%d %% %d != %d! " % [val, a[0], a[1]] + [a, b, [mod, val]].inspect if val % a[0] != a[1]
    raise "%d %% %d != %d! " % [val, a[0], a[1]] + [a, b, [mod, val]].inspect if val % b[0] != b[1]
    [mod, val]
  end[1]
end

def aoc16a input
  str = input.dup
  str = str + ?0 + str.reverse.tr("01", "10") until str.length >= 272
  str = str[0 ... 272]
  str = str.gsub(/../){|pair|pair[0] == pair[1] ? 1 : 0} until str.length.odd?
  str
end

def aoc16b input
  str = input.dup
  str = str + ?0 + str.reverse.tr("01", "10") until str.length >= 35651584
  str = str[0 ... 35651584]
  str = str.gsub(/../){|pair|pair[0] == pair[1] ? 1 : 0} until str.length.odd?
  str
end

def aoc17a
  input="udskfozm"
  layer = [[input, 0, 0]]
  loop do
    layer.map! do |node, x, y|
      return node[input.length .. -1] if x == 3 && y == 3
      doors_open = Digest::MD5.hexdigest(node)[0..3].chars.map{|c|c.to_i(16) > 10}
      [
        ([node + ?U, x, y - 1] if y > 0 && doors_open[0]),
        ([node + ?D, x, y + 1] if y < 3 && doors_open[1]),
        ([node + ?L, x - 1, y] if x > 0 && doors_open[2]),
        ([node + ?R, x + 1, y] if x < 3 && doors_open[3])
      ].compact
    end.flatten!(1)
  end
end

def aoc17b
  input="udskfozm"
  layer = [[input, 0, 0]]
  r = nil
  loop.with_index do |_, i|
    return r if layer.empty?
    layer.map! do |node, x, y|
      if x == 3 && y == 3
        r = i
        []
      else
        doors_open = Digest::MD5.hexdigest(node)[0..3].chars.map{|c|c.to_i(16) > 10}
        [
          ([node + ?U, x, y - 1] if y > 0 && doors_open[0]),
          ([node + ?D, x, y + 1] if y < 3 && doors_open[1]),
          ([node + ?L, x - 1, y] if x > 0 && doors_open[2]),
          ([node + ?R, x + 1, y] if x < 3 && doors_open[3])
        ].compact
      end
    end.flatten!(1)
  end
end

def aoc18a input
  r = 0
  line = input.chars
  40.times do
    r += line.count ?.
    line = [?., *line, ?.].each_cons(3).map{|l, m, r| (l == r) ? ?. : ?^}
  end
  r
end

def aoc18b input
  r = 0
  line = input.chars
  400000.times do
    r += line.count ?.
    line = [?., *line, ?.].each_cons(3).map{|l, m, r| (l == r) ? ?. : ?^}
  end
  r
end

def aoc19a input
  2 * input.to_i - 2 ** input.to_i.bit_length + 1
end

def aoc19b input
  x = input.to_i
  top = 3 ** (x.to_s(3).length - 1)
  case
  when x == top then top
  when x < 2 * top then x - top
  else 2 * x - 3 * top
  end
end

def aoc20a input
  ranges = input.scan(/(\d+)\-(\d+)/).map{|x|x.map(&:to_i)}.sort
  ranges = ranges.reduce [] do |acc, range|
    if acc.empty? || acc.last[1] + 1 < range[0]
      acc << range.dup
    else
      acc.last[1] = range[1] if acc.last[1] < range[1]
    end
    acc
  end
  ranges[0][0] > 0 ? 0 : ranges[0][1] + 1
end

def aoc20b input
  ranges = input.scan(/(\d+)\-(\d+)/).map{|x|x.map(&:to_i)}.sort
  ranges = ranges.reduce [] do |acc, range|
    if acc.empty? || acc.last[1] + 1 < range[0]
      acc << range.dup
    else
      acc.last[1] = range[1] if acc.last[1] < range[1]
    end
    acc
  end
  2**32 - ranges.map{|x|x[1] - x[0] + 1}.reduce(&:+)
end

def aoc21a input
  str = "abcdefgh"
  input.each_line do |line|
    case line
    when /swap position (\d+) with position (\d+)/
      str[$1.to_i], str[$2.to_i] = str[$2.to_i], str[$1.to_i]
    when /swap letter (.) with letter (.)/
      i, j = str.index($1), str.index($2)
      str[i], str[j] = $2, $1
    when /rotate (left|right) (\d+) steps?/
      str = if $1 == "left"
        str[$2.to_i .. -1] + str[0 ... $2.to_i]
      else
        str[-$2.to_i .. -1] + str[0 ... -$2.to_i]
      end
    when /rotate based on position of letter (.)/
      i = str.index $1
      i += i >= 4 ? 2 : 1
      i %= str.length
      str = str[-i .. -1] + str[0 ... -i]
    when /reverse positions (\d+) through (\d+)/
      str[$1.to_i .. $2.to_i] = str[$1.to_i .. $2.to_i].reverse
    when /move position (\d+) to position (\d+)/
      c = str[$1.to_i]
      str[$1.to_i] = ""
      str[$2.to_i, 0] = c
    else
      raise "unrecognized instruction #{line}"
    end
    puts "#{line} -> #{str}"
  end
  str
end

def aoc21b input
  str = "fbgdceah"
  input.lines.reverse.each do |line|
    case line
    when /swap position (\d+) with position (\d+)/
      str[$1.to_i], str[$2.to_i] = str[$2.to_i], str[$1.to_i]
    when /swap letter (.) with letter (.)/
      i, j = str.index($1), str.index($2)
      str[i], str[j] = $2, $1
    when /rotate (left|right) (\d+) steps?/
      str = if $1 == "right"
        str[$2.to_i .. -1] + str[0 ... $2.to_i]
      else
        str[-$2.to_i .. -1] + str[0 ... -$2.to_i]
      end
    when /rotate based on position of letter (.)/
      i = [1, 1, 6, 2, 7, 3, 0, 4][str.index $1]
      str = str[i .. -1] + str[0 ... i]
  when /reverse positions (\d+) through (\d+)/
      str[$1.to_i .. $2.to_i] = str[$1.to_i .. $2.to_i].reverse
    when /move position (\d+) to position (\d+)/
      c = str[$2.to_i]
      str[$2.to_i] = ""
      str[$1.to_i, 0] = c
    else
      raise "unrecognized instruction #{line}"
    end
    puts "#{line} -> #{str}"
  end
  str
end

def aoc22a input
  nodes = input.scan(%r{/dev/grid/node-x\d+-y\d+ +\d+T +(\d+)T +(\d+)T +\d+\%})
  nodes.map{|a| a[0] == "0" ? 0 : nodes.count{|b| b[1].to_i >= a[0].to_i}}.reduce(&:+)
end

def aoc22b input
  walls = Set[]
  gap_start = nil
  size = {x: 0, y: 0}
  nodes = input.scan(%r{
    /dev/grid/node-x(?<x>\d+)-y(?<y>\d+)
    \s+(?<size>\d+)T[ ]+(?<used>\d+)T[ ]+(?<avail>\d)+T[ ]+\d+\%
  }x).map{|n| n.map! &:to_i; {x: n[0], y: n[1], size: n[2], used: n[3], avail: n[4]}}

  nodes.each do |node|
    size[:x] = node[:x] if size[:x] < node[:x]
    size[:y] = node[:y] if size[:y] < node[:y]
    case
    when node[:used] > 200 && node[:avail] < 60
      walls << [node[:x], node[:y]]
      p walls
    when node[:size] > 100
      raise "big node with space #{node}"
    when node[:size] < 80
      raise "small node #{node}"
    when node[:used] == 0
      if gap_start
        raise "two gaps #{gap_start} and #{node}"
      else
        gap_start = node
      end
    when node[:used] < 60 || node[:used] > 80
      raise "unexpected payload in #{node}"
    end
  end

  puts

  prev_lvls = Set[]
  this_lvl = Set[[[gap_start[:x], gap_start[:y]], [size[:x], 0]]]
  loop.with_index do |_, depth|
    raise "nothing found" if this_lvl.size == 0
    this_lvl = Set[*this_lvl.map do |gap, goal|
      return depth if goal == [0, 0]
      [
        ([gap[0] - 1, gap[1]] if gap[0] > 0),
        ([gap[0] + 1, gap[1]] if gap[0] < size[:x]),
        ([gap[0], gap[1] - 1] if gap[1] > 0),
        ([gap[0], gap[1] + 1] if gap[1] < size[:y]),
      ].reject do |new_gap|
        new_gap.nil? || walls.include?(new_gap)
      end.map do |new_gap|
        [new_gap, (new_gap == goal ? gap : goal)]
      end.reject do |new_node|
        prev_lvls.include? new_node
      end
    end.flatten(1)]
    prev_lvls.merge this_lvl
  end
end

def aoc23ab part, input
  input.gsub!(
    /^cpy (\S+) ([abcd])\ninc ([abcd])\ndec \2\njnz \2 -2$/,
    "add \\1 \\3\nrst \\2\nnop\nnop"
  )
  input.gsub!(
    /^add (\S+) ([abcd])\n((?:rst [abcd]\n|nop\n){3})dec ([abcd])\njnz \4 -5\n/,
    "mul \\1 \\4\nadd \\4 \\2\nrst \\4\n\\3"
  )

  puts input
  gets

  toggle_tbl = {cpy: :jnz, inc: :dec, dec: :inc, jnz: :cpy, tgl: :inc}
  regs = {:ip => 0, :a => (part == :b ? 7 : 12), :b => 0, :c => 0, :d => 0}
  code = input.lines.map do |line|
    line.split(" ").map {|oa| (oa =~ /-?\d+/) ? oa.to_i : oa.to_sym}
  end

  while regs[:ip] < code.length
    op, *args = code[regs[:ip]]
    case op
    when :cpy
      if args[1].is_a? Integer then nil
      elsif args[0].is_a? Integer then regs[args[1]] = args[0]
      else regs[args[1]] = regs[args[0]]
      end
    when :inc then regs[args[0]] += 1
    when :dec then regs[args[0]] -= 1
    when :jnz
      arg0 = (args[0].is_a? Integer) ? args[0] : regs[args[0]]
      arg1 = (args[1].is_a? Integer) ? args[1] : regs[args[1]]
      unless arg0 == 0
        regs[:ip] += (arg1 - 1)
        raise "jump into optimized code" unless toggle_tbl[code[regs[:ip]][0]]
      end
    when :tgl
      ip_toggled = regs[:ip] + regs[args[0]]
      if ip_toggled < code.length
        code[ip_toggled][0] = toggle_tbl[code[ip_toggled][0]]
        raise "toggle inside optimized code" unless code[ip_toggled][0]
      end

    when :nop then nil
    when :rst then regs[args[0]] = 0
    when :add then regs[args[1]] += ((args[0].is_a? Integer) ? args[0] : regs[args[0]])
    when :mul then regs[args[1]] *= ((args[0].is_a? Integer) ? args[0] : regs[args[0]])

    else
      raise "unknown instruction #{op} at #{regs[:ip]}"
    end
    regs[:ip] += 1
  end
  regs[:a]
end

def aoc24ab part, input
  input = input.lines
  pois = []

  input.each.with_index do |line, y|
    line.each_char.with_index do |char, x|
      pois[char.to_i] = [x, y] if char =~ /\d/
    end
  end

  dists = pois.map do |from|
    dists_row = Array.new pois.count
    prev_lvl = Set[]
    this_lvl = Set[from]

    catch :got_dists do
      loop.with_index do |_, dist|
        next_lvl = Set[*this_lvl.map do |x, y|
          dists_row[input[y][x].to_i] = dist if input[y][x] =~ /\d/
          throw :got_dists, dists_row unless dists_row.any? &:nil?
          [[x, y + 1], [x, y - 1], [x + 1, y], [x - 1, y]].reject do |x, y|
            prev_lvl.include?([x, y]) || input[y][x] == "#"
          end
        end.flatten(1)]
        prev_lvl, this_lvl = this_lvl, next_lvl
      end
    end
  end

  [*1 ... pois.length].permutation.map do |poi_order|
    path = (part == :b) ? [0, *poi_order, 0] : [0, *poi_order]
    path.each_cons(2).map{|i, j| dists[i][j]}.reduce(&:+)
  end.min
end

def aoc25ab part, input
  input.gsub!(
    /^cpy (\S+) ([abcd])\ninc ([abcd])\ndec \2\njnz \2 -2$/,
    "add \\1 \\3\nrst \\2\nnop\nnop"
  )
  input.gsub!(
    /^add (\S+) ([abcd])\n((?:rst [abcd]\n|nop\n){3})dec ([abcd])\njnz \4 -5\n/,
    "mul \\1 \\4\nadd \\4 \\2\nrst \\4\n\\3"
  )

  toggle_tbl = {cpy: :jnz, inc: :dec, dec: :inc, jnz: :cpy, tgl: :inc}
  code = input.lines.map do |line|
    line.split(" ").map {|oa| (oa =~ /-?\d+/) ? oa.to_i : oa.to_sym}
  end
  loop.with_index do |_, init_a|
    puts "trying #{init_a}"
    catch :next_a do
      regs = {:ip => 0, :a => init_a, :b => 0, :c => 0, :d => 0}
      out_ix = 0
      timeout = 0
      while regs[:ip] < code.length
        regs[:ip] = 0 if regs[:ip] < 0
        op, *args = code[regs[:ip]]
        case op
        when :cpy
          if args[1].is_a? Integer then nil
          elsif args[0].is_a? Integer then regs[args[1]] = args[0]
          else regs[args[1]] = regs[args[0]]
          end
        when :inc then regs[args[0]] += 1
        when :dec then regs[args[0]] -= 1
        when :jnz
          arg0 = (args[0].is_a? Integer) ? args[0] : regs[args[0]]
          arg1 = (args[1].is_a? Integer) ? args[1] : regs[args[1]]
          unless arg0 == 0
            regs[:ip] += (arg1 - 1)
            if regs[:ip] > 0 && regs[:ip] < code.length && !toggle_tbl[code[regs[:ip]][0]]
              raise "jump into optimized code (to #{regs[:ip]} by #{arg1})"
            end
          end
        when :tgl
          ip_toggled = regs[:ip] + regs[args[0]]
          if ip_toggled < code.length
            code[ip_toggled][0] = toggle_tbl[code[ip_toggled][0]]
            raise "toggle inside optimized code" unless code[ip_toggled][0]
          end

        when :nop then nil
        when :rst then regs[args[0]] = 0
        when :add then regs[args[1]] += ((args[0].is_a? Integer) ? args[0] : regs[args[0]])
        when :mul then regs[args[1]] *= ((args[0].is_a? Integer) ? args[0] : regs[args[0]])

        when :out
          out_val = (args[0].is_a? Integer) ? args[0] : regs[args[0]]
          print (0..9).include?(out_val) ? out_val : "(#{out_val})"
          unless out_val == out_ix % 2
            puts "", "incorrect output"
            throw :next_a
          end
          out_ix += 1
          print "\e[1K\e[50D" if out_ix % 50 == 0
          return init_a if out_ix == 5000
        else
          raise "unknown instruction #{op} at #{regs[:ip]}"
        end
        regs[:ip] += 1
        timeout += 1
        if timeout == 1e6
          puts "", "no output for 1_000_000 ticks"
          throw :next_a
        end
      end
    end
  end
ensure puts ""
end

def getLines
  r = ""
  loop do
    g = gets
    if g.chomp.empty?
      return r
    else
      r += g
    end
  end
end

input = getLines
time = Time.now
p aoc25ab :b, input
puts "result took %.2f seconds to compute" % (Time.now - time)
