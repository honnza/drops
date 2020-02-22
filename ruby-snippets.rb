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

x = generate_group(rand(144), x=[*0..143].shuffle, &digitwise_sum(12))

#x=[*0..99].shuffle; x.each_slice(20).map{|x| puts x.join " "}
puts x.map{|x|(x/12).to_s(12)}.join; puts x.map{|x|(x%12).to_s(12)}.join
#x.map{|e|puts e;g=gets; x<< g+e.to_s if g[/./]}

h="";loop{h+=rand(2).to_s;print h;h="" if gets[/./]}

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

def relax_rescale(grid, strength, suppressed_modes = [->{1}])
  fmt = lambda do |i, j, val|
    eps = 1e-10
    min, max = (i-1..i+1).map{|ii| (j-1..j+1).map{|jj| grid[ii][jj]}}.flatten.compact.minmax
    case val
    when nil then " " * 18
    when    -eps ..     eps then "\e[37;1m%.16f\e[0m"
    when min-eps .. min+eps then "\e[31;1m%.16f\e[0m"
    when max-eps .. max+eps then "\e[36m%.16f\e[0m"
    when -0.5 .. 0  then "\e[33;1m%.16f\e[0m"
    when 0 .. 0.5  then "\e[36;1m%.16f\e[0m"
    else "\e[32;1m%.16f\e[0m"
    end % val &.abs
  end
  grid = grid.split(/[\n\/]/).map{|row| row.chars.map{|c|
    {?- => -1.0, ?. => 0.0, ?+ => 1.0, ?? => rand}[c]
  }} if grid.is_a? String
  grid.each{|row| row << nil}
  grid << []
  
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

  old_grids = Set.new
  loop.with_index do |_, t|
    energy = 0
    grid = (0 ... grid.size).map do |i|
      (0 ... grid[i].size).map &lambda{|j|
        return nil if grid[i][j].nil?
        delta = [[-1, 0], [0, -1], [0, 1], [1, 0]].map do |di, dj|
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

    cout = [""]
    grid.each.with_index{|row, i| cout << row.map.with_index{|val, j| fmt[i, j, val]}.join(" ").rstrip}
    cout << ""
    cout << "@#{t}"
    cout << "energy = %.16f" % [energy]
    cout << "delta_1 = %.16f" % [(1 - scale) / strength]
    cout << "suppression factors = %p" % [suppression_factors.map{|f| "%.1e" % f}]
    # \e[A moves cursor up; \e[?25l hides it; \e[?25h shows it again
    print "\e[#{cout.size}A\e[?25l" if t > 1
    puts cout.join("\n")

    r = grid.map{|row| row.map{|val| val&.round(16)}}
    return grid.map{|row| row.map{|val| val&.round(13)}} if old_grids.include? r
    old_grids << r
    #sleep 0.1
  end
ensure
  print "\e[0m\e[?25h"
end
