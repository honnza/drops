require 'set'

class IntcodeNoInputException < Exception; end

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
        raise IntcodeNoInputException if input.empty?
        write[1, input.shift]
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
  
  run_intcode(xs, stick).lazy.each_slice(3).each do |x, y, type|
    screen[[x, y]] = type
    p type  if x < 0
    paddle_at = x if type == 3
    if type == 4
      #p_screen[]
      #sleep 0.1
      stick << (x <=> paddle_at)
    end
  end
rescue
  p screen
  p $!
end
#day13b IO::read("github/drops/aoc2019/day13.in").split(",").map(&:to_i)

def day14a(recipes, fuel = 1)
  recipes = recipes.map{|recipe| recipe.map{|parts| Hash[parts.map(&:reverse)]}}
  makeable = ["ORE"]
  schedule = []
  orders = Hash.new(0); orders["FUEL"] = fuel
  until recipes.empty?
    recipe = recipes.find{|recipe| recipe.first.keys.all?{|key| makeable.include? key}}
    recipes.delete recipe
    recipe.last.keys.each{|key| makeable << key}
    schedule << recipe
  end
  schedule.reverse_each do |recipe|
    count = recipe.last.keys.map{|key| orders[key].fdiv(recipe.last[key]).ceil}.max
    [count, recipe]
    recipe.first.keys.each{|key| orders[key] += count * recipe.first[key]}
  end
  orders["ORE"]
end
#day14a IO::read("github/drops/aoc2019/day14.in").lines.map{|l| l.split("=>").map{|h| h.split(", ").map{|p| ck=p.split(" "); [ck[0].to_i, ck[1]]}}}

def day14b(recipes)
  ore_1 = day14a(recipes)
  p [1, ore_1]
  fuel_est = (1e12 / ore_1).floor
  loop do 
    ore = day14a(recipes, fuel_est)
    p [fuel_est, ore]
    fuel_est += ((1e12 - ore) / ore_1).floor
    fuel_est += 1 if ore + ore_1 > 1e12
    return fuel_est - 1  if ore > 1e12
  end
end
#day14b IO::read("github/drops/aoc2019/day14.in").lines.map{|l| l.split("=>").map{|h| h.split(", ").map{|p| ck=p.split(" "); [ck[0].to_i, ck[1]]}}}

def day15(xs, part)
  in_pipe = []
  out_pipe = run_intcode(xs, in_pipe)
  
  dir_op = {0+1i => 1, 0-1i => 2, -1+0i => 3, 1+0i => 4}
  loc_visits = Hash.new(0)
  status_log = Hash.new(" ")
  status_log[0+0i] = "<"
  loc = 0+0i
  
  loop do 
    choices = p dir_op.keys.map{|dir| [loc_visits[loc + dir], rand,  dir]}.sort
    next_visits, _, dir = choices[0]
    loc_visits[loc] += 1
    break if loc_visits[loc] == Float::INFINITY
    if choices[1][0] == Float::INFINITY
      loc_visits[loc] = Float::INFINITY
      neighbors = choices[1..3].map{|_, _, dir| status_log[loc + dir]}.join
      status_log[loc] = neighbors =~ /^[#-]+$/ ? "-" : '+' if status_log[loc] == "."
    end
    in_pipe << dir_op[dir]
    case out_pipe.next
    when 0 then status_log[loc + dir] = "#"; loc_visits[loc + dir] = Float::INFINITY
    when 1 then status_log[loc + dir] = "." unless status_log.has_key?(loc + dir); loc += dir
    when 2 then status_log[loc + dir] = ">"; loc += dir
    else raise "invalid status reply"
    end
    
    row_minmax = status_log.keys.map(&:imaginary).minmax
    col_minmax = status_log.keys.map(&:real).minmax
    Range.new(*row_minmax).to_a.reverse_each do |row|
      puts " " + Range.new(*col_minmax).map{|col|
        at = col + row * 1i
        at == loc ? "@" : status_log[at]
      }.join
    end
    sleep 0.1
  end
  
  return status_log.values.count("+") + 1 if part == :a
  
  next_oxygen = status_log.select{|k, v| v == ">"}.keys
  (0..).find do |t|
    last_oxygen = next_oxygen.uniq
    next_oxygen = []
    last_oxygen.each do |loc|
      status_log[loc] = "O"
      dir_op.keys.each do |dir|
        next_oxygen << loc + dir if status_log[loc + dir] =~ /[^#O]/
      end
    end

    row_minmax = status_log.keys.map(&:imaginary).minmax
    col_minmax = status_log.keys.map(&:real).minmax
    Range.new(*row_minmax).to_a.reverse_each do |row|
      puts " " + Range.new(*col_minmax).map{|col|
        at = col + row * 1i
        at == loc ? "@" : status_log[at]
      }.join
    end
    puts t
    sleep 0.1

    next_oxygen.empty?
  end
end
#day15 IO::read("github/drops/aoc2019/day15.in").split(",").map(&:to_i), :b

def day16a (rounds, digits, skip: 0)
  fft = lambda do |v| 
    w = v.size
    sums = [0] * (skip + 1); v[skip..].each{|v| sums << v + sums.last}
    [nil] * skip + (skip + 1 .. w).map do |n| 
      #for pattern n, we are asked to sum v[(4k+1)n] ... v[(4k+2)n]
      #that is, partial sum before (4k+2)n minus partial sum before (4k+1)n
      #and subtract all v[(4k+3)n] ... v[(4k+4)n]
      #similarly derived from partial sums
      #so before each k*n, we want to subtract, add, add, subtract
      #the corresponding partial sums, and then once at the very end to pair the previous one
      r = 0
      (1..).each{|k| 
        break if n * k > w && k.odd?
        #p [n, k, [-1, -1, 1, 1][k % 4] * sums[[k * n - 1, w].min]]
        r += [-1, -1, 1, 1][k % 4] * sums[[k * n - 1, w].min]
      }
      r.abs % 10
    end
  end
  rounds.times{|i| digits = fft[digits]; p [digits.join[0..99], i]}
  digits.join
end
#day16a 100, IO::read("github/drops/aoc2019/day16.in").chars.map(&:to_i)

def day16b(rounds, digits)
  day16a(rounds, digits * 10000, skip: digits.take(7).join.to_i)[0..7]
end
#day16b 100, IO::read("github/drops/aoc2019/day16.in").chars.map(&:to_i)

def day17a(xs)
  cam = run_intcode(xs).map(&:chr).join.lines
  (1 .. cam.size - 2).map do |y|
    (1 .. cam[y].size - 2).map do |x|
      x * y unless [cam[y][x], cam[y][x-1], cam[y][x+1], cam[y-1][x], cam[y+1][x]].include?(".")
    end.compact
  end.flatten.sum
end
#day17a IO::read("github/drops/aoc2019/day17.in").split(",").map(&:to_i)

def day17b(xs)
  cam = run_intcode(xs.dup).map(&:chr).join.lines
  puts cam
  bot_y = cam.find_index{|row| row =~ /[<>v^]/}
  bot_x = cam[bot_y].index /[<>v^]/
  bot_dir = cam[bot_y][bot_x]
  path = ""
  
  left = {"^" => "<", "<" => "v", "v" => ">", ">" => "^"}
  right = {"^" => ">", ">" => "v", "v" => "<", "<" => "^"}
  dx = {"<" => -1, ">" => 1, "^" => 0, "v" => 0}
  dy = {"<" => 0, ">" => 0, "^" => -1, "v" => 1}
  
  loop do 
    case
    when cam[bot_y + dy[bot_dir]][bot_x + dx[bot_dir]] == "#" then nil
    when cam[bot_y + dy[left[bot_dir]]][bot_x + dx[left[bot_dir]]] == "#"
      bot_dir = left[bot_dir]; path += "L,"
    when cam[bot_y + dy[right[bot_dir]]][bot_x + dx[right[bot_dir]]] == "#"
      bot_dir = right[bot_dir]; path += "R,"
    else break
    end
    bot_x += dx[bot_dir]; bot_y += dy[bot_dir]; path += "1,"
  end
  path.gsub!(/1(,1)+/){$&.length / 2 + 1}
  puts path
  
  find_compression = lambda do |lengths, unset|
    if unset.size > 0
      (2..21).each do |len|
        new_lengths = lengths.merge({unset[0] => len})
        catch(unset[0]){find_compression[new_lengths, unset[1..]]}
      end
    else
      path_ix = 0
      main = ""
      funcs = {}
      loop do
        key = lengths.keys.find do |key|
          funcs[key] ||= path[path_ix, lengths[key]]
          unless funcs[key][-1] == ","
            throw key
          end
          path[path_ix, lengths[key]] == funcs[key]
        end
        if key
          main += key.to_s + ","
          path_ix += funcs[key].length
          if main.length > 21
            throw lengths.keys.last
          end
        elsif path_ix == path.length
          throw :found, [main, *funcs.values]
        else 
          throw lengths.keys.last
        end
      end
      nil
    end
  end
  
  lines = catch(:found){find_compression[{}, [:A, :B, :C]]} + ["n,"]
  lines.each{|l| l[/,$/] = "\n"}
  
  xs[0] = 2
  puts lines.join
  out_pipe = run_intcode(xs, lines.join.chars.map(&:ord))
  loop{c = out_pipe.next; print c.chr}
end
#day17b IO::read("github/drops/aoc2019/day17.in").split(",").map(&:to_i)

def day18a(xss)
  mid_y = xss.find_index{|xs| xs =~ /@/}
  mid_x = xss[mid_y].index("@")
  dx = {"<" => -1, ">" => 1, "^" => 0, "v" => 0}
  dy = {"<" => 0, ">" => 0, "^" => -1, "v" => 1}
  item_paths = {}
  scan = lambda do |path, y, x|
    return if xss[y][x] == "#"
    item_paths[xss[y][x]] = path unless xss[y][x] == "."
    scan[path + "<", y, x - 1] if path.length != 1 && path[-1] != ">" && x != mid_x + 1
    scan[path + ">", y, x + 1] if path.length != 1 && path[-1] != "<" && x != mid_x - 1
    scan[path + "^", y - 1, x] if path.length != 0 && path[-1] != "v" && y != mid_y + 1
    scan[path + "v", y + 1, x] if path.length != 0 && path[-1] != "^" && y != mid_y - 1
  end
  scan["", mid_y, mid_x]
  
  item_paths.sort.each{|k, v| p [k, v.length, v]}
  
  behind = Hash.new {|h, (key, lock)| h[[key, lock]] = item_paths[lock.upcase] && item_paths[key].start_with?(item_paths[lock.upcase])}
  dists = Hash.new do |h, (x, y)| 
    px=item_paths[x].dup; py = item_paths[y].dup
    (px.slice!(1); py.slice!(1)) if px[1] == py[1]
    (px.slice!(0); py.slice!(0)) while px[0] == py[0]
    p [x, y, px.length, py.length]
    h[[x, y]] = px.length + py.length
  end
  
  sequence = Hash.new do |h, (last, unused)|
    if unused.empty?
      h[[last, unused]] = [0, last]
    else
      next_keys = unused.reject{|k| unused.any?{|l| behind[[k, l]]}}
      options = p next_keys.map{|k| sequence[[k, unused - [k]]]}
      subcost, suborder = options.min
      h[[last, unused]] = [dists[[last, suborder[0]]] + subcost, last + suborder]
    end
  end
  sequence[["@", item_paths.keys.select{|k| k =~ /[a-z]/}, 0]]
end
#day18a IO::read("github/drops/aoc2019/day18.in").lines

def day19a(xs)
  out = 0
  (0..49).map do |y|
    puts((0..49).map do |x|
      r = run_intcode(xs.dup, [x, y]).next
      out += r
      " #"[r]
    end.join)
  end
  out
end
#day19a IO::read("github/drops/aoc2019/day19.in").split(",").map(&:to_i)

def day19b(xs)
  x = 0; y = 0
  loop do
    case 
    when run_intcode(xs.dup, [x + 99, y]).next == 0 then y += 1
    when run_intcode(xs.dup, [x, y + 99]).next == 0 then x += 1
    else return x * 10000 + y
    end
  end
end
#day19b IO::read("github/drops/aoc2019/day19.in").split(",").map(&:to_i)

def day21a(xs)
  r = run_intcode(xs, <<END.gsub("; ", "\n").chars.map(&:ord))
NOT C J; NOT B T; OR T J; NOT A T; OR T J; AND D J; WALK
END
  puts r.peek > 127 ? r.next : r.map(&:chr).join
end
#day21a IO::read("github/drops/aoc2019/day21.in").split(",").map(&:to_i)

def day21b(xs)
  r = run_intcode(xs, <<END.gsub("; ", "\n").chars.map(&:ord))
NOT C J; NOT B T; OR T J; NOT A T; OR T J; AND D J; NOT A T; OR H T; AND T J; RUN
END
  puts r.peek > 127 ? r.next : r.map(&:chr).join
end
#day21b IO::read("github/drops/aoc2019/day21.in").split(",").map(&:to_i)

def day22a(xs)
  card_at = 2019
  xs.each do |x|
    case x
    when /deal into new stack/
      card_at = 10006 - card_at
    when /cut (-?\d+)/
      card_at = (card_at - $1.to_i) % 10007
    when /deal with increment (\d+)/
      card_at = (card_at * $1.to_i) % 10007
    end
  end
  card_at
end
#day22a IO::read("github/drops/aoc2019/day22.in").lines

def day22b(xs)
  modulus = 119315717514047
  zero_at = 0
  increment = 1
  xs.each do |x|
    case x
    when /deal into new stack/
      zero_at = modulus - zero_at - 1
      increment = modulus - increment - 1 
    when /cut (-?\d+)/
      zero_at = (zero_at - $1.to_i) % modulus
    when /deal with increment (\d+)/
      zero_at = (zero_at * $1.to_i) % modulus
      increment = (increment * $1.to_i) % modulus
    end
    p [zero_at, increment, x]
  end

  zero_n_times = 0
  increment_n = 1
  n = 0
  101741582076661.to_s(2).chars.each do |bit|
    # 0 -> z -> zi+z
    zero_n_times = (zero_n_times * increment_n + zero_n_times) % modulus
    increment_n = increment_n * increment_n % modulus
    n *= 2 
    if bit == '1'
      zero_n_times = (zero_n_times * increment + zero_at) % modulus
      increment_n = increment_n * increment % modulus
      n += 1
    end
    p [n, zero_n_times, increment_n]
  end

  quot = [nil, nil]
  rem = [modulus, increment_n]  
  c_m = [1, 0]
  c_i = [0, 1]
  while rem.last > 0
    quot << rem[-2] / rem[-1]
    rem << rem[-2] - quot.last * rem[-1]
    c_m << c_m[-2] - quot.last * c_m[-1]
    c_i << c_i[-2] - quot.last * c_i[-1]
    p [quot.last, rem.last, c_m.last, c_i.last, c_i.last * increment_n + c_m.last * modulus]
  end
  
  # ci + z = 2020 => c = (2020-z)/i
  (2020 - zero_n_times) * c_i[-2] % modulus
end
#day22b IO::read("github/drops/aoc2019/day22.in").lines

def day23(xs, part)
  in_pipes = (0..49).map{|x| [x]}
  out_pipes = in_pipes.map{|pipe| run_intcode(xs.dup, pipe)}
  packet_queues = Array.new(50){[]}
  nat_data = nil
  nat_prev_y = nil
  
  loop.with_index do |_, t|
    if packet_queues.all?(&:empty?) && t > 0
      return nat_prev_y if nat_prev_y == nat_data[1]
      nat_prev_y = nat_data[1]
      packet_queues[0] << nat_data
    end
    
    (0..49).each do |ip_addr|
      if t == 0 || packet_queues[ip_addr].size > 0
        xy = packet_queues[ip_addr].shift
        in_pipes[ip_addr].concat(xy || [-1])

        loop do
          begin
            out_addr = out_pipes[ip_addr].next
            if out_addr != -1
              out_xy = [out_pipes[ip_addr].next, out_pipes[ip_addr].next]
              packet_queues[out_addr] &.<< out_xy
            end
            if out_addr == 255
              return out_xy[1] if part == :a
              nat_data = out_xy
            end
          rescue IntcodeNoInputException
            break
          end
        end
      end
    end
  end
end
#day23 IO::read("github/drops/aoc2019/day23.in").split(",").map(&:to_i), :b

def day24a(state)
  state = state.tr("\n", "").tr(".#", "01").reverse.to_i(2)
  prev_states = Set.new [state]
  loop do
    ss = [state << 5 & 0b11111_11111_11111_11111_00000,
          state << 1 & 0b11110_11110_11110_11110_11110,
          state >> 1 & 0b01111_01111_01111_01111_01111,
          state >> 5 & 0b00000_11111_11111_11111_11111]
    ns = 0
    (0..24).each do |bit| 
      c = ss.map{|s| s[bit]}.sum
      ns |= 1 << bit if c == 1 || c + state[bit] == 2
    end
    p ns.to_s(2).ljust(25, '0').reverse.scan(/...../)
    return ns if prev_states.include? ns
    prev_states << ns
    state = ns
  end
end
#day24a "#####...###..#.#....#...#"

def day24b(state)
  neighs_0 = Hash.new{|h, k| h[k] = []}
  neighs_0[?H] = [[1, ?A], [1, ?B], [1, ?C], [1, ?D], [1, ?E]]
  neighs_0[?L] = [[1, ?A], [1, ?F], [1, ?K], [1, ?P], [1, ?U]]
  neighs_0[?N] = [[1, ?E], [1, ?J], [1, ?O], [1, ?T], [1, ?Y]]
  neighs_0[?R] = [[1, ?U], [1, ?V], [1, ?W], [1, ?X], [1, ?Y]]
  %w{A B C D E}.each {|k| neighs_0[k] << [-1, ?H]}
  %w{A F K P U}.each {|k| neighs_0[k] << [-1, ?L]}
  %w{E J O T Y}.each {|k| neighs_0[k] << [-1, ?N]}
  %w{U V W X Y}.each {|k| neighs_0[k] << [-1, ?R]}
  "ABCDFGHIKNPQRSUVWX".chars.each{|k| neighs_0[k] << [0, k.next]; neighs_0[k.next] << [0, k]}
  "ABCDEFGIJKLNOPQRST".chars.each{|k| neighs_0[k] << [0, (k.ord+5).chr]; neighs_0[(k.ord+5).chr] << [0, k]}
  neighs = ->((tier, tile)){neighs_0[tile].map{|dt, t| [tier + dt, t]}}
  
  state = (?A .. ?Y).zip(state.chars).map{|c, s| [0, c] if s == ?#}.compact
  200.times do |t|
    neigh_count = Hash.new{|h,k| h[k] = 0}
    state.each{|bug| neighs[bug].each{|neigh| neigh_count[neigh] += 1}}
    state = Set.new neigh_count.keys.select{|tile| neigh_count[tile] == 1 || neigh_count[tile] == 2 && !state.include?(tile)}
    p [t, state.min, state.max, state.size]
  end
  state.size
end
#day24b "#####...###..#.#....#...#"

def day25a(xs)
  in_pipe = []
  out_pipe = run_intcode(xs, in_pipe)
  last_line = ""
  
  loop do
    last_line += out_pipe.next.chr
    if last_line[-1] == "\n"
      puts last_line
      in_pipe.concat gets.chars.map(&:ord) if last_line == "Command?\n"
      last_line = ""
    end
  end
end
#day25a IO::read("github/drops/aoc2019/day25.in").split(",").map(&:to_i)
