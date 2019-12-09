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
day9a IO::read("github/drops/aoc2019/day9.in").split(",").map(&:to_i)

def day9a(xs)
  p run_intcode(xs, [2]).to_a
end
day9a IO::read("github/drops/aoc2019/day9.in").split(",").map(&:to_i)
