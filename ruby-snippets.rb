def gc x; (255 * x**(1/2.2)).round; end

def find_collatz(x, triple_even = true)
  paths_to = {x => [""]}
  search_queue = [x]
  search_queue.each do |n|
    return paths_to[n].map do |path|
      path.split("*").map{|part| part.length.to_s(16)}.join.gsub(/00(?=(00)*[^0])/, ";")
    end if n == 1
    [(n/2 if n.even?), (3*n+1 if n.odd? || triple_even)].compact.each do |nn|
      npaths = paths_to[n].map{|npath| npath + (nn > n ? "*" : "/")}
      if paths_to[nn]
        paths_to[nn].concat npaths if (paths_to[nn][0].length == npaths[0].length)
      else
        paths_to[nn] = npaths
        search_queue << nn
      end
    end
  end
  return nil
end

def combinations_3(x, y, z, n)
  0.upto(n) do |n_x|
    0.upto(n-n_x) do |n_y|
      n_z = n - n_x - n_y
      c = 1
      [[x, n_x], [y, n_y], [z, n_z]].each do |(d, n_d)| 
        c *= d.downto(d - n_d + 1).reduce(1, :*) / 1.upto(n_d).reduce(1, :*)
      end
      print "#{c} "
    end
    puts
  end
end

def ext_euclid(m, n)
  q = [nil, nil]
  r = [m,n]
  c_n = [1, 0]
  c_m = [0, 1]
  loop.with_index(2) do |_, i|
    q[i] = r[i-2] / r[i-1]
    [r, c_m, c_n].each{|col| col[i] = col[i-2] - col[i-1] * q[i]}
    return [q, r, c_m, c_n].transpose if r[i] == 0
  end
end

def snake_boxing(w,h)
  snakes = [*0...h].product([*0...w]).map{|pt| [pt]}
  free_ends = snakes.map{|s| [s[0], s]} * 2
  loop do
    pair = free_ends.combination(2).to_a.shuffle.find{|((x1, y1), s1), ((x2, y2), s2)|
      # s1.product(s2).count{|((x1, y1), (x2, y2))| (x1-x2).abs + (y1-y2).abs == 1} == 1 &&
      s1 != s2 && (x1-x2).abs + (y1-y2).abs == 1
    }
    return snakes if pair.nil?
    
    pair[0][1].reverse! unless pair[0][1].last == pair[0][0]
    pair[1][1].reverse! unless pair[1][1].first == pair[1][0]
    new_snake = pair[0][1] + pair[1][1]
    snakes << new_snake

    pair.each do |es|
      snakes.delete(es[1])
      free_ends.delete_at(free_ends.find_index(es))
      free_ends.find{|(e2, s2)| s2 == es[1]}[1] = new_snake
    end
  end
end

def generate_hypergrid(total, &op)
  r = [rand(total)]
  until r.size >= total
    gen = rand(total)
    r |= r.map{|x| op.(x, gen)}
    p [gen, r.size]
  end
  r
end

def generate_group(first, gens, &op)
  gen_at = gens.map{0}
  elems = [first]
  loop do
    (0...gens.count).find do |gen_ix|
      new_elem = op.(elems[gen_at[gen_ix]], gens[gen_ix])
      unless elems.include?(new_elem)
        elems << new_elem
        gen_at[gen_ix] += 1
      end
    end or return elems
  end
end

def digitwise_sum(b)
  f = Proc.new{|x, y| (x + y) % b + (x < b && y < b ? 0 : f[x/b, y/b] * b)}
end

# x = generate_group(rand(144), x=[*0..143].shuffle, &digitwise_sum(12))

# #x=[*0..99].shuffle; x.each_slice(20).map{|x| puts x.join " "}
# puts x.map{|x|(x/12).to_s(12)}.join; puts x.map{|x|(x%12).to_s(12)}.join
# #x.map{|e|puts e;g=gets; x<< g+e.to_s if g[/./]}

# h="";loop{h+=rand(2).to_s;print h;h="" if gets[/./]}

def inversion_count_histogram(n)
  r = Hash.new {|h, k| h[k] = 0}
  [*1 .. n].permutation{|x| r[x.combination(2).count{|c| c[0] > c[1]}] += 1}
  p r.values.join(", ")
  p r.values.sum

end

#algorithm taken from https://oeis.org/A008302
def inversion_count_table(n)
  res = Hash.new do |h, (n, k)|
    # puts "evaluating [%d, %d]" % [n, k]
    h[[n, k]] = if n == 1
                  (k == 0) ? 1 : 0
                elsif k < 0
                  0
                else
                  res[[n, k-1]] + res[[n-1, k]] - res[[n-1, k-n]]
                end
    # puts "res[[%d, %d]] = %d" % [n, k, res[[n, k]]]
    res[[n, k]]
  end
  
  (0..).lazy.map{|k| res[[n, k]]}.take_while{|r| r > 0}.force
end

def grid_sampler(h, w, torus: false, neighbor_count: 8, show: true)
  neighbors = [
    [-1, 0], [0, -1], [0, 1], [1, 0],
    [-1, -1], [-1, 1], [1, -1], [1,1]
  ][0 ... neighbor_count]
  
  considered = (0...h).map{|y| (0...w).map{|x| [y, x]}}.flatten(1)
  rejected = []
  
  until considered.empty?
    s = considered.sample
    (0...h).each{|y| puts (0...w).map{|x|
      case
      when s == [y, x] then "\#"
      when considered.include?([y, x]) then " "
      when rejected.include?([y, x]) then "x"
      else "-"
      end
    }.join}
    p s
    case gets.strip
    when "x"
      considered.delete s
      rejected << s
    when '-'
      considered.delete s
    when '' 
      neighbors.each{|n| 
        s2 = s.zip(n).map{|i, j| i + j}
        next if s2.any?{|i| i < 0} || s2.first >= h || s2.last >= w
        considered << s2 unless rejected.include? s2
      }
    else puts "unknown input"
    end
  end
end

def outercalate(str, scale = 1)
  outercalate_line = ->x{" #{x.join} ".gsub(/(.)(?=(.))/){$1 * scale + (($1 + $2) == "  " ? " " : ".")}[1...-1].chars}
  ary = str.split(%r[[\n\/]]).map(&:chars)
  ary.map! &outercalate_line
  ary = ary.transpose
  ary.map! &outercalate_line
  ary = ary.transpose
  ary.map(&:join).join("\n")
end

def intercalate(str, scale = 1)
  intercalate_line = ->x{" #{x.join} ".gsub(/(.)(?=(.))/){$1 * scale + (($1 + $2) =~ / / ? " " : ".")}[1...-1].chars}
  ary = str.split(%r[[\n\/]]).map(&:chars)
  ary.map! &intercalate_line
  ary = ary.transpose
  ary.map! &intercalate_line
  ary = ary.transpose
  ary.map(&:join).join("\n")
end

def upscale(str, n=2)
  upscale_line = ->x{x.flat_map{|x| [x]*n}}
  ary = str.split(%r[[\n\/]]).map(&:chars)
  ary.map! &upscale_line
  ary = ary.transpose
  ary.map! &upscale_line
  ary = ary.transpose
  ary.map(&:join).join("\n")
end

require "io/console"
def relax_rescale(grid, f: 0.1, n: :n4, s: [])
  strength = f; neighborhood = n; suppressed_modes = s.dup
  grid = grid.split(/[\n\/]/).map{|row| row.chars.map{|c|
    {?- => -1.0, ?. => 0.0, ?+ => 1.0, ?? => rand, ?e => rand / 1e10}[c]
  }} if grid.is_a? String
  precision = IO.console.winsize[1] / grid.map(&:length).max - 3
  precision = 16 if precision > 16
  (puts "warning: precision = #{precision}"; precision = 1) if precision < 1

  fmt = lambda do |i, j, val|
    logval = Math.log10(val.abs) / -10 if val
    case val
    when nil then " " * (precision + 2)
    when -1 .. 0  then "\e[38;2;255;#{(255*(1+val)).round};#{(255*logval).round}m%.*f\e[0m"
    when  0 .. 1  then "\e[38;2;#{(255*logval).round};#{(255*(1-val)).round};255m%.*f\e[0m"
    else "\e[32;1m%.*f\e[0m"
    end % [precision, val&.abs]
  rescue
    "\e[32;1m%.*f\e[0m" % [precision, val&.abs]
  end
  
  suppressed_modes << ->{1}
  suppressed_modes = suppressed_modes.map do |mode|
    case mode
    when Array then mode.map{|row| row.dup}
    when Proc 
      (0...grid.size).map{|y| (0...grid[y].size).map{|x| grid[y][x] && mode.call(*[y, x][0...mode.arity])}}
    end
  end
  suppressed_modes.each do |mode|
    norm = mode.map{|row| row.map{|val| val && val * val}}.flatten.compact.sort.sum ** 0.5
    mode.each{|row| row.map!{|val| val && val / norm}}
  end

  neighborhood = case neighborhood
    when :n4 then [[-1, 0], [0, -1], [0, 1], [1, 0]]
    when :n8 then [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
    when :n_knight then [[-2, -1], [-2, 1], [-1, 2], [1, 2], [2, 1], [2, -1], [1, -2], [-1, -2]]
    else neighborhood
  end

  neighborhood.map{|_, j| j.abs}.max.times{grid.each{|row| row << nil}}
  neighborhood.map{|i, _| i.abs}.max.times{grid << []}

  old_grids = Set.new
  prev_frame = nil
  loop.with_index do |_, t|
    energy = 0
    grid = (0 ... grid.size).map do |i|
      (0 ... grid[i].size).map &lambda{|j|
        return nil if grid[i][j].nil?
        delta = neighborhood.map do |di, dj|
          grid[i+di][j+dj] - grid[i][j] if grid[i+di] && grid[i+di][j+dj]
        end.compact.sort.sum
        energy += delta.abs
        grid[i][j] + delta * strength
      }
    end

    suppression_factors = suppressed_modes.map do |mode|
      factor = (0 ... grid.size).map do |i|
        (0 ... grid[i].size).map {|j| mode[i][j] && grid[i][j] && mode[i][j] * grid[i][j]}
      end.flatten.compact.sum
      (0 ... grid.size).each do |i|
        (0 ... grid[i].size).each do |j|
          grid[i][j] -= factor * mode[i][j] if grid[i][j] && mode[i][j]
        end
      end
      factor
    end

    scale = grid.flatten.compact.minmax.map(&:abs).max
    grid.each{|row| row.map!{|val| val &./ scale}}

    r = [energy, strength]

    if(t == 0 || Time.now - prev_frame > 0.1 || old_grids.include?(r))
      cout = [""]
      grid.each.with_index{|row, i| cout << row.map.with_index{|val, j| fmt[i, j, val]}.join(" ").rstrip}
      cout << ""
      cout << "@#{t}"
      cout << "energy = %.16f" % [energy]
      cout << "delta_1 = %.16f" % [(1 - scale) / strength]
      # cout << "suppression factors = %p" % [suppression_factors.map{|f| "%.1e" % f}]
      # \e[A moves cursor up; \e[?25l hides it; \e[?25h shows it again
      cout << "\e[#{cout.size}A\e[?25l" unless old_grids.include? r
      print cout.join("\n")
	  
	  return nil if scale < 1e-10
      return {
	    mode: grid.map{|row| row.map{|val| val&.round(13)}},
		delta_1: (1 - scale) / strength
	  } if old_grids.include? r
      prev_frame = Time.now
    end
    old_grids << r
  end
ensure
  print "\e[0m\e[?25h\n"
end

def foo(x, limit = nil, n: :n4, f: 0.1, grid: nil)
  # generate channels
  xs = x.split(/[\/\n]/)
  modes = []
  accepted_modes = []
  loop do  
    modes << relax_rescale(x, n: n, s: modes.map{|x| x[:mode]}, f: f)
	break if modes.last.nil? || limit.is_a?(Float) && modes.last[:delta_1] >= limit

    dot = xs.zip(modes.last[:mode]).map do |cs, ms|
      cs.chars.zip(ms).map{|c, m|m.nil? ? 0 : m * {"-" => -1, "+" => 1, "." => 0, "?" => rand, "e" => 0}.fetch(c, c)}
    end.flatten.sum
	len = modes.last[:mode].map{|ms| ms.map{|m| m &.** 2}}.flatten.compact.sort.sum ** 0.5
	
	if dot.abs > 1e-7 || limit
      puts "mode #{accepted_modes.size + 1} strength = #{dot} / #{len} = #{dot / len}", "---"
      accepted_modes << [dot / len, accepted_modes.size + 1, modes.last[:mode]]
	else
	  puts "rejected mode: dot product = #{dot}"
    end
	puts "strongest modes: #{accepted_modes.sort.reverse.map{|_, ix, _| ix}}" if limit
	sleep [dot / len * 2, 1].max
	break if (limit.nil? || limit.is_a?(Integer)) && accepted_modes.size >= (limit || 3)
  end

  accepted_modes.sort.reverse.each{|strength, ix, _| p [strength, ix]} if limit
  sleep 5 if limit
  
  # transpose into pixels
  g, r, b = (limit.nil? ? accepted_modes : accepted_modes.max(3)).map(&:last)
  plan = (0 ... xs.size).map do |i|
    (0 ... xs[i].size).map do |j|
      if xs[i][j] != ' '
        [i, j] + [r, g, b].map {|c| [c.nil? ? 127 : (c[i][j]*128+128).floor, 255].min}
      end
    end
  end.flatten(1).compact

  d1 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| (cx - cy).abs}.sum}
  d2 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| (cx - cy) ** 2}.sum ** 0.5}
  d16 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| 
    high, low = (cx - cy).abs.divmod(16)
	4 * high + [low, 20 - low].min
  }.sum}

  # nearest neighbor heuristic from all sources biased for space-continuity
  todo = plan
  plan = todo.map do |first|
    plan = [first]
    todo_rest = todo.dup
    todo_rest.delete first
    until todo_rest.empty?
      el = todo_rest.filter do |el|
	    plan.any?{|el2| el[0..1].zip(el2).all?{|x, y| (x - y).abs <= 1}}
	  end.min_by{|el| d16[plan.last, el]}
      plan << el
      todo_rest.delete el
    end
    plan
  end.min_by{|plan| plan.each_cons(2).map{|x, y| d2[x, y]}.sum}

  # 2.5-opt: flip strands and move individual nodes
  [d16].each do |d|
    loop do
      prev_plan = plan.dup
      (0 ... plan.length).each do |elix|
        (0 .. plan.length).each do |gapix|
          next if (elix - gapix).abs < 2
          dol = elix == 0 ? 0 : d[plan[elix - 1], plan[elix]]
          dor = elix == plan.length - 1 ? 0 : d[plan[elix], plan[elix + 1]]
          dog = gapix == 0 || gapix == plan.length ? 0 : d[plan[gapix - 1], plan[gapix]]
          dnl = gapix == 0 ? 0 : d[plan[gapix - 1], plan[elix]]
          dnr = gapix == plan.length ? 0 : d[plan[elix], plan[gapix]]
          dng = elix == 0 || elix == plan.length - 1 ? 0 : d[plan[elix - 1], plan[elix + 1]]
          if dol + dor + dog > dnl + dnr + dng
            plan = plan.dup
            plan.insert(gapix, plan[elix])
            plan.delete_at(elix > gapix ? elix + 1 : elix)
            puts "moved [#{elix}] to #{gapix}; saved #{dol + dor + dog - dnl - dnr - dng} points"
          end
        end
      end
	  (puts "---"; redo) if prev_plan != plan
      (0 .. plan.length - 2).reverse_each do |lix|
        (lix + 2 .. plan.length).each do |rix|
          dol = lix == 0 ? 0 : d[plan[lix - 1], plan[lix]]
          dor = rix == plan.length ? 0 : d[plan[rix - 1], plan[rix]]
          dnl = lix == 0 ? 0 : d[plan[lix - 1], plan[rix - 1]]
          dnr = rix == plan.length ? 0 : d[plan[lix], plan[rix]]
          if dol + dor > dnl + dnr
            plan = plan[0 ... lix] + plan[lix ... rix].reverse + plan[rix ...]
            puts "flipped [#{lix} ... #{rix}]; saved #{dol + dor - dnl - dnr} points"
          end
        end
      end
      break if prev_plan == plan
      puts "---"
    end
  end

  # display the results
  bitmap = xs.map{|x| " " * x.size}
  plan.each{|i, j, _, _, _| bitmap[i][j] = grid && (i % grid == 1 || j % grid == 1) ? "," : "."}
  plan.chunk{|el| el[2..4]}.each do |rgb, els|
    puts [("#{els.count}x" if els.count > 1),rgb.inspect].compact.join " "
    els.each{|el| bitmap[el[0]][el[1]] = "o"}
    bitmap.each{|row| puts row.gsub(/./, '\& ')}
    els.each{|el| bitmap[el[0]][el[1]] = "@"}
    gets
  end
end

def bisect(min, max, &fn)
  mid = (min+max)/2
  case
  when mid == min || mid == max || yield(mid) == 0
    mid
  when yield(min) * yield(mid) < 0
    bisect(min, mid, &fn)
  when yield(mid) * yield(max) < 0
    bisect(mid, max, &fn)
  else
    raise ArgumentError, "cannot bisect: " + [min, max].inspect
  end
end
