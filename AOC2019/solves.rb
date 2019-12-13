def run_intcode(mem, input = [])
  ip = 0; rbo = 0
  Enumerator.new do |output|
    loop do
      param_modes = mem[ip].digits.drop(2)
      read = lambda do |pos|
        case param_modes[pos - 1] || 0
        when 0 then mem[mem[ip + pos] || 0] || 0
        when 1 then mem[ip + pos] || 0
        when 2 then mem[(mem[ip + pos] || 0) + rbo] || 0
        else 
          puts "unkonwn param mode at #{ip}"
          p [param_modes, pos]
        end
      end
      write = lambda do |pos, val|
        case param_modes[pos - 1] || 0
        when 0 then mem[mem[ip + pos] || 0] = val
      # when 1 then mem[ip + pos] = val # guaranteed unused
        when 2 then mem[(mem[ip + pos] || 0) + rbo] = val
        else 
          puts "unkonwn param mode at #{ip}"
          p [param_modes, pos]
        end
      end
      
      case mem[ip] % 100
      when 1
        write[3, read[1] + read[2]]
        ip += 4
      when 2
        write[3, read[1] * read[2]]
        ip += 4
      when 3
        raise "read from empty input" if input.empty?
        write[1, input.pop]
        ip += 2
      when 4
        output.yield read[1]
        ip += 2
      when 5
        ip = (read[1] != 0) ? read[2] : ip + 3
      when 6
        ip = (read[1] == 0) ? read[2] : ip + 3
      when 7
        write[3, read[1] < read[2] ? 1 : 0]
        ip += 4
      when 8
        write[3, read[1] == read[2] ? 1 : 0]
        ip += 4
      when 9
        rbo += read[1]
        ip += 2
      when 99
        break
      else
        puts "unknown opcode at #{ip}"
        break
      end
    end
  end
end

def day1a(xs)
  xs.map{|x| x/3 - 2}.sum
end
#day1a IO::read("github/drops/aoc2019/day1.in").lines.map(&:to_i)

def day1b(xs)
  f = ->x{x < 9 ? 0 : x/3 - 2 + f[x/3-2]}
  xs.map(&f).sum
end
#day1b IO::read("github/drops/aoc2019/day1.in").lines.map(&:to_i)

def day2a(xs)
  mem = xs.dup
  mem[1] = 12
  mem[2] = 2
  run_intcode(mem)
  mem[0]
end
#day2a IO::read("github/drops/aoc2019/day2.in").split(",").map(&:to_i)

def day2b(xs)
  (0..99).each do |noun|
    (0..99).each do |verb|
      mem = xs.dup
      mem[1] = noun
      mem[2] = verb
      run_intcode(mem)
      puts "%02d%02d is a correct answer" % [noun, verb] if mem[0] == 19690720
    end
  end
end
#day2b IO::read("github/drops/aoc2019/day2.in").split(",").map(&:to_i)

def day3(xss, part)
  traces = xss.map do |xs|
    trace = []
    cx = 0; cy = 0
    xs.each do |x|
      m = x.match /(.)(.+)/
      m[2].to_i.times do
        case m[1]
        when 'D' then cy -= 1
        when 'L' then cx -= 1
        when 'R' then cx += 1
        when 'U' then cy += 1
        end
        trace << [cx, cy]
      end
    end
    trace
  end
  case part
  when :a
    traces.inject(&:&).map{|x, y| x.abs + y.abs}.min
  when :b
    traces.inject(&:&).map{|i| traces.map{|t| t.find_index(i)+1}.sum}.min
  end
end
#day3 IO::read("github/drops/aoc2019/day3.in").lines.map{|l| l.split(",")}, :b

def day4a(from, to)
  cur = from.to_s
  r = 0
  until cur > to.to_s
    pairs = cur.chars.each_cons(2)
    r += 1 if pairs.any?{|x, y| x == y} && pairs.all?{|x, y| x <= y}
    cur = cur.next
  end
  r
end
#day4a(137683, 596253)

def day4b(from, to)
  cur = from.to_s
  r = 0
  until cur > to.to_s
    pairs = cur.chars.each_cons(2)
    runs = cur.scan /((.)\2*)/
    r += 1 if pairs.all?{|x, y| x <= y} && runs.any?{|m| m[0].length == 2}
    cur = cur.next
  end
  r
end
#day4b(137683, 596253)

def day5a(xs); p run_intcode(xs, [1]).to_a; end
#day5a IO::read("github/drops/aoc2019/day5.in").split(",").map(&:to_i)

def day5b(xs); p run_intcode(xs, [5]).to_a; end
#day5b IO::read("github/drops/aoc2019/day5.in").split(",").map(&:to_i)

def day6a(xss)
  tree = Hash[xss.map(&:reverse)]
  depth = ->v{tree[v] ? depth[tree[v]] + 1 : 0}
  tree.keys.map(&depth).sum
end
#day6a IO::read("github/drops/aoc2019/day6.in").lines.map{|l| l.chomp.split(")")}

def day6b(xss)
  tree = Hash[xss.map(&:reverse)]
  path = ->v{tree[v] ? [v] + path[tree[v]] : [v]}
  
  path_san = path["SAN"]
  path_you = path["YOU"]
  (path_san.pop; path_you.pop) while path_san.last == path_you.last
  p path_san
  p path_you
  path_san.length + path_you.length - 2
end
#day6b IO::read("github/drops/aoc2019/day6.in").lines.map{|l| l.chomp.split(")")}

def day7a(xss)
  test_subtree = lambda do |cfgs, st_in|
    cfgs.map do |cfg|
      st_out = run_intcode(xss.dup, [cfg, st_in]).first
      indent = "  " * (5 - cfgs.size)
      cfgs_left = cfgs - [cfg]
      puts indent + "amp(%d, %d) => %d; %s remaining" % [cfg, st_in, st_out, cfgs_left.inspect]
      cfgs_left.empty? ? st_out : test_subtree[cfgs_left, st_out]
    end.max
  end
  test_subtree[[0, 1, 2, 3, 4], 0]
end
#day7a IO::read("github/drops/aoc2019/day7.in").split(",").map(&:to_i)

def day7b(xss)
  [5, 6, 7, 8, 9].permutation.map do |cfgs|
    in_pipes = cfgs.map{|cfg| [cfg]}
    out_pipes = in_pipes.map{|pipe| run_intcode(xss.dup, pipe)}
    feedback = 0
    iter_count = 0
    loop do
      in_pipes[0] << feedback
      in_pipes[1] << out_pipes[0].next
      in_pipes[2] << out_pipes[1].next
      in_pipes[3] << out_pipes[2].next
      in_pipes[4] << out_pipes[3].next
      feedback = out_pipes[4].next
      iter_count += 1
    end
    p [cfgs, iter_count, feedback]
    feedback
  end.max
end
#day7b IO::read("github/drops/aoc2019/day7.in").split(",").map(&:to_i)

def day8a(w, h, xs)
  xs.scan(/.{#{w*h}}/).map{|l| [l.count('0'), l.count('1') * l.count('2')]}.min.last
end
#day8a 25, 6, IO::read("github/drops/aoc2019/day8.in").chomp

def day8b(w, h, xs)
  layers = xs.scan /.{#{w*h}}/
  (0...h).each {|y| puts (0...w).map{|x| layers.map{|l| l[w*y+x]}.find{|l| l !='2'}}.join.tr("01", "# ")}
end
#day8b 25, 6, IO::read("github/drops/aoc2019/day8.in").chomp

def day9a(xs)
  p run_intcode(xs, [1]).to_a
end
#day9a IO::read("github/drops/aoc2019/day9.in").split(",").map(&:to_i)

def day9a(xs)
  p run_intcode(xs, [2]).to_a
end
#day9a IO::read("github/drops/aoc2019/day9.in").split(",").map(&:to_i)

def day10a(xss)
  locations = xss.map.with_index do |xs, i|
    xs.map.with_index{|x, j| [i, j] if x == "#"}
  end.flatten(1).compact
  locations.map do |loc|
    locations.map do |loc2|
      dx = loc[0] - loc2[0]
      dy = loc[1] - loc2[1]
      gcd = dx.gcd(dy)
      [dx / gcd, dy / gcd] if gcd > 0
    end.compact.uniq.count
  end.max
end
#day10a IO::read("github/drops/aoc2019/day10.in").lines.map{|l| l.chomp.chars}

def day10b(asteroid_ix, xss)
  dir = lambda do |loc, loc2|
    dx = loc2[0] - loc[0]
    dy = loc2[1] - loc[1]
    gcd = dx.gcd(dy)
    case
    when dx == 0 && dy < 0 then [0, Float::INFINITY]
    when dx > 0 then [1, Rational(dy, dx)]
    when dx == 0 && dy > 0 then [2, Float::INFINITY]
    when dx < 0 then [3, Rational(dy, dx)]
    end
  end
  
  sqdist = lambda do |loc, loc2|
    dx = loc[0] - loc2[0]
    dy = loc[1] - loc2[1]
    dx * dx + dy * dy
  end

  locations = xss.map.with_index do |xs, i|
    xs.map.with_index{|x, j| [j, i] if x == "#"}
  end.flatten(1).compact
  laser_at = locations.map do |loc|
    lcount = locations.map{|loc2| dir[loc, loc2]}.compact.uniq.count
    [lcount, loc]
  end.max[1]
  locations.delete laser_at
  locations.map!{|loc| [loc, dir[laser_at, loc], sqdist[laser_at, loc]]}
  
  laser_dir = [-1]
  loop.with_index(1) do |_, ix|
    next_target = locations.sort_by{|l, d, s| [(d <=> laser_dir) > 0 ? 0 : 1, d, s]}.first
    locations.delete next_target
    laser_dir = next_target[1]
    return next_target[0][0] * 100 + next_target[0][1] if ix == asteroid_ix
  end
end
#day10b 200, IO::read("github/drops/aoc2019/day10.in").lines.map{|l| l.chomp.chars}

def day11a(xs)
  in_pipe = []
  out_pipe = run_intcode xs, in_pipe
  
  robot_at = 0i
  robot_dir = 1i
  paint_log = Hash.new{0}
  
  loop do 
    in_pipe << paint_log[robot_at]
    paint_log[robot_at] = out_pipe.next
    p [paint_log.size, robot_at, paint_log[robot_at]]
    robot_dir *= out_pipe.next == 1 ? 1i : -1i
    robot_at += robot_dir
  end
  
  paint_log.size
end
#day11a IO::read("github/drops/aoc2019/day11.in").split(",").map(&:to_i)

def day11b(xs)
  in_pipe = []
  out_pipe = run_intcode xs, in_pipe
  
  robot_at = 0i
  robot_dir = 1i
  paint_log = Hash.new{0}
  paint_log[0i] = 1
  
  loop do 
    in_pipe << paint_log[robot_at]
    paint_log[robot_at] = out_pipe.next
    p [paint_log.size, robot_at, paint_log[robot_at]]
    robot_dir *= out_pipe.next == 1 ? -1i : 1i
    robot_at += robot_dir
  end
  
  row_minmax = paint_log.keys.map(&:imaginary).minmax
  col_minmax = paint_log.keys.map(&:real).minmax
  
  Range.new(*row_minmax).to_a.reverse_each do |row|
    puts Range.new(*col_minmax).map {|col| ".#"[paint_log[col + row * 1i]]}.join
  end
end
#day11b IO::read("github/drops/aoc2019/day11.in").split(",").map(&:to_i)

def day12a(steps, moons)
  len = ->v{v.map(&:abs).sum}
  moons.map!{|x| {pos: x, vel: [0, 0, 0]}}
  
  steps.times do |ix|
    puts "", "@#{ix}"
    moons.each{|m1| moons.each{|m2| [0, 1, 2].each{|d| m1[:vel][d] += m2[:pos][d] <=> m1[:pos][d]}}}
    moons.each{|m| [0, 1, 2].each{|d| m[:pos][d] += m[:vel][d]}; p m}
  end
  
  moons.map{|m| len[m[:pos]] * len[m[:vel]]}.sum
end
#day12a 1000, [[-6, -5, -8], [0, -3, -13], [-15, 10, -11], [-3, -8, 3]]

def day12b(moons)
  moons.transpose.map do |xs|
    poss = xs.dup
    vels = xs.map{0}
    
    period = (1..).find do |t|
      poss.each_with_index{|p1, i1| poss.each{|p2| vels[i1] += p2 <=> p1}}
      vels.each_with_index{|v, i| poss[i] += v}
      poss == xs && vels.all?{|v| v == 0}
    end
    p period
  end.reduce(&:lcm)
end
#day12b [[-6, -5, -8], [0, -3, -13], [-15, 10, -11], [-3, -8, 3]]

def day13a(xs)
  screen = Hash.new{|h, k| 0}
  run_intcode(xs).each_slice(3).map{|x, y, type| screen[[x, y]] = type}
  screen.values.count(2)
end
#day13a IO::read("github/drops/aoc2019/day13.in").split(",").map(&:to_i)

def day13b(xs)
  xs[0] = 2
  stick = []
  paddle_at = 0
  screen = {}
  p_screen = lambda do
    puts "", screen[[-1, 0]], ""
    w = screen.keys.map(&:first).max
    h = screen.keys.map(&:last).max
    (0..h).each{|y| puts (0..w).map{|x| screen[[x, y]]}.join.tr("0-4", " #+\\-o")}
  end
  
  run_intcode(xs, stick).lazy.each_slice(3).map{|x|p x}.each do |x, y, type|
    screen[[x, y]] = type
    paddle_at = x if type == 3
    if type == 4
      p_screen[]
      sleep 0.1
      stick << (x <=> paddle_at)
    end
  end
rescue
  p screen
  p $!
end
day13b IO::read("github/drops/aoc2019/day13.in").split(",").map(&:to_i)
