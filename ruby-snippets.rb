require "io/console"
require "matrix"
def gc x; (255 * x ** (1/2.2)).round; end

class Array; include Comparable; end

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

def generate_hypercube(total, &op)
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

def generate_lattice(dims, first = nil, gens = [])
  gens_at = []
  gen_stats = []
  elems = [first || dims.map{|d| rand d}]
  loop do
    gen_ix = gens_at.find_index{|gen_at| gen_at < elems.count}
    if gen_ix
      new_elem = elems[gens_at[gen_ix]]
        .zip(gens[gen_ix]).map{|x, y| x + y}
      if new_elem.zip(dims).all? {|e, d| (0...d).include? e} && !elems.include?(new_elem)
        elems |= [new_elem]
        gen_stats[gen_ix] += 1
        if elems.count == dims.reduce(&:*)
          p gens.zip gen_stats
          return elems
        end
      end
      gens_at[gen_ix] += 1
    else
      gens << dims.map{|d| rand (-d+1)..(d-1)}
      gens_at << 0
      gen_stats << 0
    end
  end
end

def gen_latin_square(size, steps = size ** 3)
  # algorithm taken from
  # http://sedici.unlp.edu.ar/bitstream/handle/10915/42155/Documento_completo.pdf?sequence=1
  # originally descrirbed in terms of an incidence cube
  # starting from a particular square, we do a series of steps:
  #
  # First we pick a random cell [px, py] and a new value pz;
  # let [ox, py] and [px, oy] be the cell in which pz resides and oz be the current value of [px, py]
  # remove pz from [ox, py] and [px, oy] and add oz there
  # remove oz from [px, py] and [ox, oy] and add pz there
  # this generally results in a situation in which [px, py] has two values at the same time,
  # and another one negative one time. We call this value a borrow, and store one of the values in sx/sy/sz
  #
  # If there is a borrow, we instead choose [px, py, pz] to be the borrow,
  # [ox, py], [px, oy] randomly one of the two cells where pz lies, and oz one of the values in [px, pz].
  # either way, the order is to remove old seconds (ppo), return old borrow (ppp),
  # create new borrow (ooo), create new seconds (poo)

  xy = [*0 ... size].map{|i| [*0 ... size].map{|j| (i+j) % size}}
  xz = [xy[0].dup] + xy[1..].map(&:dup).reverse
  yz = xz.map(&:dup)
  bx = nil; by = nil; bz = nil;
  sx = nil; sy = nil; sz = nil;

  rem = lambda do |x, y, z|
    if xy[x][y] != z && [x, y, z] != [bx, by, sz]
      if xz[x][z] == y || yz[y][z] == x || [x, y, z] == [bx, sy, bz] || [x, y, z] == [sx, by, bz]
        raise "borrow consistency error removing [#{x}, #{y}, #{z}]"
      end
      raise "can't remove [#{x}, #{y}, #{z}]: already borrowing" unless bx.nil? && by.nil? && bz.nil?
      bx = x; by = y; bz = z; return
    end

    case
    when [x, y, z] == [bx, by, sz]
      sz = nil
    when [x, y] == [bx, by] && xy[x][y] == z
      xy[x][y] = sz
      sz = nil
    when xy[x][y] == z
      xy[x][y] = nil
    else
      raise "error removing [#{x}, #{y}, #{z}] from xy"
    end

    case
    when [x, y, z] == [bx, sy, bz]
      sy = nil
    when [x, z] == [bx, bz] && xz[x][z] == y
      xz[x][z] = sy
      sy = nil
    when xz[x][z] == y
      xz[x][z] = nil
    else
      raise "error removing [#{x}, #{y}, #{z}] from xz"
    end

    case
    when [x, y, z] == [sx, by, bz]
      sx = nil
    when [y, z] == [by, bz] && yz[y][z] == x
      yz[y][z] = sx
      sx = nil
    when yz[y][z] == x
      yz[y][z] = nil
    else
      raise "error removing [#{x}, #{y}, #{z}] from yz"
    end
  end

  add = lambda do |x, y, z|
    if [x, y, z] == [bx, by, bz]
      raise "can't repay borrow [#{x}, #{y}, #{z}]: still in use" unless sx.nil? && sy.nil? && sz.nil?
      bx = nil; by = nil; bz = nil
      return
    end

    case
    when xy[x][y].nil? then xy[x][y] = z
    when [x, y] == [bx, by] && sz.nil? then sz = z
    else "error adding [#{x}, #{y}, #{z}] to xy"
    end

    case
    when xz[x][z].nil? then xz[x][z] = y
    when [x, z] == [bx, bz] && sy.nil? then sy = y
    else "error adding [#{x}, #{y}, #{z}] to xz"
    end

    case
    when yz[y][z].nil? then yz[y][z] = x
    when [y, z] == [by, bz] && sz.nil? then sx = x
    else "error adding [#{x}, #{y}, #{z}] to yz"
    end
  end

  (0..).each do |t|
    return xy if t >= steps && bz.nil?
    
    if bz.nil?
      px = rand size; py = rand size; pz = rand size; pz = rand size while xy[px][py] == pz
      ox = yz[py][pz]; oy = xz[px][pz]; oz = xy[px][py]
    else
      px = bx; py = by; pz = bz
      ox = rand > 0.5 ? yz[py][pz] : sx
      oy = rand > 0.5 ? xz[px][pz] : sy
      oz = rand > 0.5 ? xy[px][py] : sz
    end

    rem[px, py, oz]; rem[px, oy, pz]; rem[ox, py, pz]; add[px, py, pz]
    rem[ox, oy, oz]; add[px, oy, oz]; add[ox, py, oz]; add[ox, oy, pz]
  end
end

def digitwise_sum(b)
  f = Proc.new{|x, y| (x + y) % b + (x < b && y < b ? 0 : f[x/b, y/b] * b)}
end

def generate_symmetric(dims, &orbit_f)
  dims.map{(0 ... _1).to_a}.reduce(&:product).map(&:flatten)
      .group_by(&orbit_f).values.shuffle.map(&:shuffle).flatten(1)
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

def evolve_pair_sample(grid_size, pop_size = grid_size, iter_limit = nil)
  new_gene = lambda do
    gene = Array.new grid_size
    while gene.include? nil
      x = rand gene.size
      x = rand gene.size until gene[x].nil?
      y = rand gene.size
      y = rand gene.size until gene[y].nil?
      gene[x] = y
      gene[y] = x
    end
    gene
  end

  cross = lambda do |g1, g2|
    gene = Array.new grid_size
    while gene.include? nil
      loop_start = rand gene.size
      gene_sel, gene_unsel = rand < 0.5 ? [g1, g2] : [g2, g1]
      x = loop_start
      loop do
        y = gene_sel[x]
        gene[x] = y
        gene[y] = x
        x = gene_unsel[y]
        break if x == loop_start
      end
    end
    gene
  end

  mutate = lambda do |gene_in|
    gene = gene_in.dup
    x = rand gene.size
    y = rand gene.size
    y = rand gene.size while y == x
    gene[x], gene[y] = gene[y], gene[x]
    gene[x] = x if gene[x] == y && gene[y] != x
    gene[y] = y if gene[y] == x && gene[x] != y
    gene[gene[x]] = x
    gene[gene[y]] = y
    raise "bug: \n#{gene_in} => \n#{gene}" if gene.sort != [*0...gene.size]
    gene
  end

  fitness = lambda do |gene|
    gene.map do |i1|
      i2 = gene[i1]
      gene.map do |j1|
        j2 = gene[j1]
        (i1 - j1) ** 2 + (i2 - j2) ** 2
      end.reject{_1 == 0}.min
    end.sort
  end

  compress = lambda do |text|
    text.gsub(/((.+?)(?:, \2)+)/){"#{($1.length + 2) / ($2.length + 2)}x#{$2}"}
  end

  gf_pair = Struct.new :gene, :fitness
  pop = []
  best_fitness = nil
  loop.with_index do |_, t|
    gene = nil
    if pop.size < pop_size
      gene = new_gene[]
      gene_ix = pop.size
    else
      x = rand pop_size
      y = rand pop_size
      y = rand pop_size while y == x
      z = rand pop_size
      z = rand pop_size while z == x || z == y
      if pop[x].fitness < pop[y].fitness && pop[x].fitness < pop[z].fitness
        gene = cross[pop[y].gene, pop[z].gene]
        gene = mutate[gene] if gene == pop[y].gene || gene == pop[z].gene
        gene_ix = x
      elsif pop[y].fitness < pop[z].fitness
        gene = cross[pop[x].gene, pop[z].gene]
        gene = mutate[gene] if gene == pop[x].gene || gene == pop[z].gene
        gene_ix = y
      else
        gene = cross[pop[x].gene, pop[y].gene]
        gene = mutate[gene] if gene == pop[x].gene || gene == pop[y].gene
        gene_ix = z
      end
    end

    gene_fitness = fitness[gene]
    pop[gene_ix] = gf_pair.new gene, gene_fitness
    if best_fitness.nil? || best_fitness < gene_fitness
      best_fitness = gene_fitness
      puts "@#{t} new best gene: #{gene}\nscore: #{compress[gene_fitness.to_s]}"
    end
    if rand < 0.01
      str = "@#{t} testing #{gene}"[0..IO.console.winsize[1] - 1]
      print "#{str}\e[#{str.length}D"
    end
    break if iter_limit && t >= iter_limit
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

def progress_bar progress, text = "", width = IO.console.winsize[1] - 1
  on_cells = ((width - 2) * progress.clamp(0 .. 1))
  if on_cells > text.length
    text = text.ljust(width - 2)
    text[on_cells.floor] = (0x2590 - on_cells % 1 * 8).floor.chr(Encoding::UTF_8) if on_cells % 1 != 0
    (text + "]").insert(on_cells.floor, "\e[0m")
                .insert(0, "[\e[107;30m")
  else
    bg = (on_cells % 1 * 256).floor
    fg = bg > 127 ? 30 : 97
    (text.ljust(width - 2) + "]")
      .insert(on_cells.floor + 1, "\e[0m")
      .insert(on_cells.floor, "\e[48;2;#{bg};#{bg};#{bg};#{fg}m")
      .insert(0, "[\e[107;30m")
  end
end

# https://en.m.wikipedia.org/wiki/Box%E2%80%93Muller_transform
box_muller = Enumerator.new do |y|
  loop do
    u = (-2 * Math.log(rand)) ** 0.5
    v = 2 * Math::PI * rand
    y.yield u * Math.sin(v)
    y.yield u * Math.cos(v)
  end
end
define_method(:rand_normal){box_muller.next}

def relax_rescale(grid, f: 0.1, n: :n4, s: [])
  strength = f; neighborhood = n; suppressed_modes = s.dup
  grid = grid.split(/[\n\/]/).map{|row| row.chars.map{|c|
    {?- => -1.0, ?. => 0.0, ?+ => 1.0, ?? => rand(-1.0 .. 1.0), ?e => rand(-1e-10 .. 1e-10)}[c]
  }} if grid.is_a? String
  precision = IO.console.winsize[1] / grid.map(&:length).max - 1
  dense_out = precision < 1
  precision = 16 if precision > 16
  (puts "warning: precision = #{precision}"; precision = 1) if precision < 1

  fmt_secs = lambda do |s|
    return "" unless s.finite?
    return "%.2fs" % [s] if s < 60
    m, s = s.divmod 60
    return "%dm %0.2fs" % [m, s] if m < 60
    h, m = m.divmod 60
    return "%dh %dm %0.2fs" % [h, m, s] if h < 24
    d, h = h.divmod 24
    return "%dd %dh %dm %0.2fs" % [d, h, m, s]
  end

  fmt = lambda do |i, j, val|
    rval = val.abs ** 0.5 * (val > 0 ? 1 : -1) if val
    logval = Math.log10(val.abs) / -10 if val
    c = ->x{(255 * x).clamp(0, 255).round}
    s = ("%.*f" % [precision, val.abs]).sub("0.", "").sub("1.0", "A") if val
    case val
    when nil then " " * precision
    when -1 .. 0  then "\e[38;2;255;#{c[1+rval]};#{c[logval]}m#{s}\e[0m"
    when  0 .. 1  then "\e[38;2;#{c[logval]};#{c[1-rval]};255m#{s}\e[0m"
    else "\e[32;1m#{s}\e[0m"
    end 
  rescue
    "\e[32;1m%.*f\e[0m" % [precision, val&.abs]
  end
  
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

  prev_grid = nil
  prev_frame_t = nil
  prev_max_delta = nil
  time_start = Time.now
  smooth_speed = Float::NAN
  smooth_time_est = Float::NAN
  loop.with_index do |_, t|
    energy = 0
    grid = (0 ... grid.size).map do |i|
      (0 ... grid[i].size).map &lambda{|j|
        return nil if grid[i][j].nil?
        neighbors = neighborhood.is_a?(Proc) ? neighborhood[i, j] : neighborhood
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

    max_delta = prev_grid && grid.zip(prev_grid).map do |xs, pxs|
      xs.zip(pxs).map{|x, px| (x - px).abs if x}
    end.flatten.compact.max
    t_delta = Time.now - prev_frame_t if prev_frame_t
    last_frame = t > 1 && scale < 1e-10 || (max_delta &.<= 2 * Float::EPSILON)
    
    if(t == 0 || t_delta > 0.1 || last_frame)
      if prev_max_delta
        frame_speed = (Math.log(max_delta) - Math.log(prev_max_delta)) / t_delta
        smooth_speed = if smooth_speed.finite?
          smooth_speed * 0.9 + frame_speed * 0.1
        else frame_speed end

        delta_goal = smooth_speed < 0 ? Math.log(Float::EPSILON) : 1
        time_est = (delta_goal - Math.log(max_delta)) / smooth_speed
      else
        time_est = Float::NAN
      end
      smooth_time_est = if smooth_time_est.finite?
        (smooth_time_est - t_delta).clamp(0..) * 0.9 + time_est * 0.1
      else time_est end

      cout = [""]
      grid.each.with_index{|row, i| cout << row.map.with_index{|val, j| fmt[i, j, val]}.join(dense_out ? "" : " ").rstrip}
      cout << ""
      cout << "@#{t}"
      cout << "energy = %.16f" % [energy]
      cout << "delta_1 = %.16f" % [(1 - scale) / strength]
      # cout << "suppression factors = %p" % [suppression_factors.map{|f| "%.1e" % f}]
      # \e[A moves cursor up; \e[?25l hides it; \e[?25h shows it again
      cout << progress_bar(
        Math.log(max_delta || 1) / Math.log(Float::EPSILON),
        "#{fmt_secs[Time.now - time_start]} / #{"> " if smooth_speed > 0}#{fmt_secs[Time.now - time_start + smooth_time_est]}"
      )
      print "\e[#{cout.size - 1}A\e[?25l" if prev_frame_t
      print cout.join("\n")
      return nil if last_frame && scale < 1e-10
      return {
        mode: grid.dup,
        delta_1: (1 - scale) / strength
      } if last_frame
      prev_frame_t = Time.now
      prev_max_delta = max_delta
    end
    prev_grid = grid.dup
  end
ensure
  print "\e[0m\e[?25h\n"
end

def relax_rescale_eigen(grid, n: :n4, interactive: true)
  grid = grid.split(/[\n\/]/).map{|row| row.chars.map{|c|
    {?- => -1.0, ?. => 0.0, ?+ => 1.0, ?? => rand(-1.0 .. 1.0), ?e => rand(-1e-10 .. 1e-10)}[c]
  }} if grid.is_a? String
  precision = IO.console.winsize[1] / grid.map(&:length).max - 1
  precision = 16 if precision > 16
  dense_out = precision < 1
  (puts "warning: precision = #{precision}"; precision = 1) if precision < 1

  fmt = lambda do |val|
    rval = val.abs ** 0.5 * (val > 0 ? 1 : -1) if val
    logval = Math.log10(val.abs) / -10 if val
    c = ->x{(255 * x).clamp(0, 255).round}
    str = ("%.*f" % [precision, val.abs]).sub("0.", "").sub("1.0", "A") if val
    case val
    when nil then " " * precision
    when -1 .. 0  then "\e[38;2;255;#{c[1+rval]};#{c[logval]}m#{str}\e[0m"
    when  0 .. 1  then "\e[38;2;#{c[logval]};#{c[1-rval]};255m#{str}\e[0m"
    else "\e[32;1m#{str}\e[0m"
    end 
  rescue
    "\e[32;1m%.*f\e[0m" % [precision, val&.abs]
  end

  neighborhood = case n
    when :n4 then [[-1, 0], [0, -1], [0, 1], [1, 0]]
    when :n8 then [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
    when :n_knight then [[-2, -1], [-2, 1], [-1, 2], [1, 2], [2, 1], [2, -1], [1, -2], [-1, -2]]
    else neighborhood
  end

  ix_count = 0
  ix_map = grid.map{|row| row.map{|c| (ix_count += 1; ix_count - 1) unless c.nil?}}
  in_vec = Vector[*grid.flatten.compact]

  matrix = Matrix.zero ix_count
  grid.length.times do |r|
    grid[r].length.times do |c|
      unless grid[r][c].nil?
        neighborhood.each do |dr, dc|
          if dr >= -r && dc >= -c && grid[r + dr] && grid[r + dr][c + dc]
            ix1 = ix_map[r][c]
            ix2 = ix_map[r + dr][c + dc]
            matrix[ix2, ix2] += 1
            matrix[ix1, ix2] -= 1
          end
        end
      end
    end
  end

  es = matrix.eigensystem

  Enumerator.new do |y|
    es.eigenvectors.zip(es.eigenvalues, 1..)
      .slice_when{|(_, v1, _), (_, v2, _)| v2 - v1 > 1e-10}
      .each do |modes|
      aligned_evs = [modes.map{|ev, _, _| ev * ev.dot(in_vec)}.reduce(&:+).normalize] rescue []
      (modes.count - aligned_evs.count).times do |i|
        new_mode = modes.map{|ev, _, _| ev * rand_normal}.reduce(&:+)
        aligned_evs.each{|ev| new_mode -= ev * ev.dot(new_mode)}
        aligned_evs << new_mode.normalize
      end
      aligned_evs.map!{|ev| ev / ev.map(&:abs).max}

      if interactive
        if modes.count == 1
          puts "#mode #{modes[0][2]}/#{ix_count} eigenvalue #{modes[0][1]}"
          ix_map.each{|row| puts row.map{|ix| fmt[ix && aligned_evs[0][ix]]}.join(dense_out ? "" : " ").rstrip}
        else
          puts "#modes #{modes[0][2]}..#{modes[-1][2]}/#{ix_count} eigenvalue #{modes[0][1]}"
          offset = modes[0][2] == 1 && modes.count > 3 ? 1 : 0
          g = aligned_evs[0 + offset]
          r = aligned_evs[1 + offset]
          b = aligned_evs[2 + offset] || aligned_evs[1 + offset]
          ix_map.each do |row|
            puts row.map{|ix|
              ix.nil? ? "><" : "\e[48;2;%d;%d;%dm  \e[0m" % [r[ix], g[ix], b[ix]].map{|c| (256 * (c + 1)/2).clamp(0..255)} 
            }.join
          end
        end
      end
      aligned_evs.each do |ev|
        y.yield mode: ix_map.map{|row| row.map{|ix| ix && ev[ix]}}, delta_1: modes[0][1]
      end
    end
  end
end

def cryptogram_hash(str)
  tally = str.chars.tally
  letter_map = tally.to_a.select{|k, v| k[/[a-z]/] && v >= 2}.map{|k, _| k}.zip("a"..).to_h
  str.chars.map{|c| letter_map[c] || ("_" if c[/[a-z]/]) || c}.join.gsub(/__+/){$&.length}
end

def compressed_ch(str, debug: false)
  bits = ""

  sym_count_max = str.length / 2 + 1
  sym_count_bits_long = Math::log2(sym_count_max).ceil
  sym_counts_short = 2 ** sym_count_bits_long - sym_count_max
  
  tally = str.chars.tally
  letter_map = tally.to_a.select{|k, v| k[/[a-z]/] && v >= 2}.map{|k, _| k}.zip(1..).to_h
  sym_count = letter_map.size + 1
  if sym_count_max > 0
    if sym_count <= sym_counts_short
      bits << (sym_count - 1).to_s(2).rjust(sym_count_bits_long - 1, "0")
    else
      bits << (sym_count + sym_counts_short - 1).to_s(2).rjust(sym_count_bits_long, "0")
    end
    puts bits if debug
  end

  if sym_count > 1
    sym_bits_long = Math::log2(sym_count).ceil
    syms_short = 2 ** sym_bits_long - sym_count
    str.chars.each do |char|
      sym = letter_map[char] || 0
      if sym < syms_short
        bits << sym.to_s(2).rjust(sym_bits_long - 1, "0")
      else
        bits << (sym + syms_short).to_s(2).rjust(sym_bits_long, "0")
      end
      puts bits if debug
    end
  end

  result = str.length.to_s(35)
  result.prepend("Z" * (result.length - 1))
  bits.scan(/.{1,5}/).each{|bits| result << bits.ljust(5, "0").to_i(2).to_s(32)}
  result
end

def bell_cch(str)
  pascal = []
  (0 .. str.length).each do |n|
    pascal[n] = [1]
    (1 .. n-1).each{|k| pascal[n][k] = pascal[n-1][k-1] + pascal[n-1][k]}
    pascal[n][n] = 1
  end

  # Not _the_ Bell triangle (I couldn't figure out how to use it to index partitions) but a related one.
  # Each bell[n][k] tells us how many ways to partition n elements
  # such that the first element is in a partition of size at most k.
  bell = [[1]]
  (1 .. str.length).each do |n|
    bell[n] = [0]
    (1 .. n).each{|k| bell[n][k] = bell[n][k-1] + pascal[n-1][k-1] * bell[n-k].last}
  end

  index_part = lambda do |ixes|
    return 0 if ixes == [] || ixes[0] == ixes.length - 1
    pascal[ixes[0]][ixes.count] + index_part[ixes[1..]]
  end

  index_str = lambda do |str|
    return 0 if p(str) == ""
    len = str.length
    first_part = (len - 1).downto(0).select{|ri| str[len - ri - 1] == str[0]}
    bell[len][first_part.length - 1] +
      index_part[first_part[1..]] * bell[len - first_part.length].last +
      index_str[str.chars.reject{_1 == str[0]}.join] 
  end

  'z' * (str.length.to_s(32).length - 1) + str.length.to_s(32) +
    index_str[str].to_s(32).rjust(index_str[str.gsub(/./, "a")].to_s(32).length, "0")
end

def cryptogram_hashes(strs, dictionary = nil)
  add_atom = lambda do |str|
    r = capitalize_periodic str
    r5 = r.select{_1.scan(/[A-Z]/).count == 5}
    if r5.length > 0 then r5[0] + "🧪"
    elsif r.length > 0 then r[0]
    else str
    end
  end

  def chunk_up strs, join
    return [strs.join(join)] if strs.join(join).bytes.count <= IO.console.winsize[1]
    return strs if strs.uniq.count == 1
    plx = 0
    plx += 1 while strs.map{_1[..plx].downcase}.uniq.count == 1
    chunks = strs.group_by{_1[..plx].downcase}.values.map{chunk_up _1, join}
    r = []
    while chunks.count > 1
      if chunks[0].count > 1 || chunks[1].count > 1
        r << chunks.shift
      elsif (chunks[0][0] + join + chunks[1][0]).bytes.count > IO.console.winsize[1]
        r << chunks.shift
      else
        chunks[0 .. 1] = [[chunks[0][0] + join + chunks[1][0]]]
      end
    end
    r + chunks
  end

  dict_words = File.read(dictionary).split(/\s+/) if dictionary

  strs = strs.map{|str| [bell_cch(str), compressed_ch(str), cryptogram_hash(str), str]}
  strs.group_by{_1[0][0]}.sort.each do |len, strs|
    chunks = strs.group_by(&:first).sort.map do |hash, strs|
      atomed_strs = strs.map(&:last).uniq.sort.map(&add_atom)
      atomed_strs -= dict_words if dict_words
      chunks = chunk_up atomed_strs, " "
      key_str = "#{hash} #{strs.map{_1[1]}.uniq.join "/"} #{strs.map{_1[2]}.uniq.join "/"} =>"
      if chunks == [""] 
        nil
      elsif chunks.length == 1 && (key_str + chunks[0]).bytes.count < IO.console.winsize[1]
        [key_str, *chunks].join " "
      else
        [key_str, *chunks].join "\n"
      end
    end
    puts chunk_up chunks.compact, " | "
    puts
  end
  nil
end

def capitalize_periodic str
  elements = Set.new %w{
    h                                                    he
    li be                                 b  c  n  o  f  ne
    na mg                                 al si p  s  cl ar
    k  ca   sc ti v  cr mn fe co ni cu zn ga ge as se br kr
    rb sr   y  zr nb mo tc ru rh pd ag cd in sn sb te i  xe
    cs ba   lu hf ta w  re os ir pt au hg tl pb bi po at rn
    fr ra   lr rf db sg bh hs mt ds rg cn nh fl mc lv ts og

            la ce pr nd pm sm eu gd tb dy ho er tm yb
            ac th pa u  np pu am cm bk cf es fm md no
  }

  recurse = lambda do |str|
    return [""] if str == ""
    r = []
    r += recurse[str[1...]].map{str[...1].capitalize + _1} if str.length >= 1 && elements.include?(str[...1])
    r += recurse[str[2...]].map{str[...2].capitalize + _1} if str.length >= 2 && elements.include?(str[...2])
    r
  end

  recurse[str.downcase]
end

def foo(x, limit = nil, filter: nil, n: :n4, f: 0.1, grid: nil, hicolor: false, rgb: false, png: false, eigen: false)
  # generate channels
  xs = x.split(/[\/\n]/)
  mode_gen = relax_rescale_eigen(x, n: n) if eigen
  modes = []
  accepted_modes = []
  aborted = false
  loop.with_index do |_, t|
    modes << (eigen ? mode_gen.next : relax_rescale(x, n: n, s: modes.map{|x| x[:mode]}, f: f))

    break if modes[t].nil? || limit.is_a?(Float) && modes[t][:delta_1] >= limit

    if xs.any?{|cs| cs[/\?/]} 
      dot = modes[t][:mode].flatten.compact.map{|m| m*m}.sum
      abs_dot = 1
    else
      dot = xs.zip(modes[t][:mode]).map do |cs, ms|
        cs.chars.zip(ms).map{|c, m|m.nil? ? 0 : m * {"-" => -1, "+" => 1, "." => 0, "?" => rand(-1.0 .. 1.0), "e" => 0}.fetch(c, c)}
      end.flatten.sum
      abs_dot = xs.zip(modes[t][:mode]).map do |cs, ms|
        cs.chars.zip(ms).map{|c, m|m.nil? ? 0 : m.abs * {"-" => 1, "+" => 1, "." => 0, "?" => rand(-1 .. 1), "e" => 0}.fetch(c, c)}
      end.flatten.sum
    end
    if dot < 0
      modes[t][:mode].each{|ms| ms.map!{|m| -m if m}}
      dot = -dot
    end
    if dot > 1e-7 && modes[t][:delta_1] > 1e-7
      puts "mode #{t + 1} strength = #{dot} / #{abs_dot} = #{dot / abs_dot}"
      accepted_modes << [dot, t + 1, modes[t][:mode]]
    elsif dot <= 1e-7
      puts "rejected mode #{t + 1}: dot product = #{dot}"
    else
      puts "rejected mode #{t + 1}: delta_1 = #{modes[t][:delta_1]}"
    end
    if t == modes.count - 1
      puts "strongest modes: #{accepted_modes.sort.reverse.map{|_, ix, _| ix}}" if limit
      gets unless aborted
    end
    break if limit.nil? && accepted_modes.size == 3 ||
             limit.is_a?(Integer) && t + 1 >= limit ||
             limit.is_a?(Array) && t + 1 >= limit.max
  rescue Interrupt, IRB::Abort
    puts "user abort"
    aborted = true
    break unless eigen
  end

  # transpose into pixels
  g, r, b = case limit
            when nil then accepted_modes
            when Numeric then accepted_modes.sort.reverse.each{|strength, ix, _| p [strength, ix]}.take(3)
            when Array then limit.map{|i| [modes[i-1][:mode]]}
            end.map(&:last)
  return unless g
  b = r = g unless r
  [g, r, b].each{|c| c&.each{ |ci| ci.map! {|cij| cij&.round(10)}}}
  plan = (0 ... xs.size).map do |i|
    (0 ... xs[i].size).map do |j|
      if g[i][j]
        [i, j] + [r, g, b].map do |c|
          if c.nil?
            128
          else
            c01 = case filter
            when :abs then c[i][j].abs
            when :lowcolor then (c[i][j] <=> 0) / 2.0 + 0.5
            when :thinc, :athinc
              is_thinc = lambda do |i, j|
                [[-1, 0], [0, -1], [0, 1], [1, 0]].any? do |di, dj|
                  c[i + di] && c[i + di][j + dj] &&
                  c[i][j] * c[i + di][j + dj] <= 0 &&
                  (c[i][j].abs < c[i + di][j + dj].abs &&
                  ! is_thinc[i + di, j + dj] ||
                  c[i][j].abs == c[i + di][j + dj].abs)
                end
              end
              is_thinc[i, j] ? 1 : filter == :athinc ? c[i][j].abs : 0
            when :contour, :acon
              [[-1, 0], [0, -1], [0, 1], [1, 0]].map do |di, dj|
                case
                when c[i][j] == 0 then 1
                when c[i + di].nil? || c[i + di][j + dj].nil? then 0
                when c[i][j] * c[i + di][j + dj] > 0 then 0
                else c[i + di][j + dj] / (c[i + di][j + dj] - c[i][j])
                end
              end.max.clamp(0 .. 1) ** 0.45 + (filter == :acon ? c[i][j].abs : 0)
            else
              puts "unknown filter #{filter}" if filter
              c[i][j]/2 + 0.5
            end
            (c01 * 256).floor.clamp(0, 255)
          end
        end
      end
    end
  end.flatten(1).compact

  if hicolor
    plan.map!{|i, j, r, g, b| [i, j, r & ~7, g & ~3, b & ~7]}
  end

  if png
    require 'chunky_png'
    image = ChunkyPNG::Image.new(
      plan.map{|_, j, _, _, _| j}.max + 1,
      plan.map{|i, _, _, _, _| i}.max + 1
    )
    plan.each{|i, j, r, g, b| image[j, i] = ChunkyPNG::Color.rgb(r, g, b)}
    (0 ... image.height).each do |i|
      (0 ... image.width).each do |j|
        rgba = image[j, i]
        if rgba & 0xff > 128
          rgb_str = "#{rgba >> 24 & 0xff};#{rgba >> 16 & 0xff};#{rgba >> 8 & 0xff}"
          print "\e[38;2;#{rgb_str};48;2;#{rgb_str}m##\e[0m"
        else
          print "><"
        end
      end
      puts
    end
    image.save png.to_s
    return
  end

  if rgb
    [["[-, -, %d]", 4], ["[%d, -, -]", 2], ["[-, %d, -]", 3]].each do |cf, ci|
      bitmap = xs.map{|x| " " * x.size}
      plan.each{|i, j, _, _, _| bitmap[i][j] = grid && (i % grid == grid - 2 || j % grid == grid - 2) ? "," : "."}
      plan.group_by{|el| el[ci]}.to_a.sort.each do |cv, els|
        puts [("#{els.count}x" if els.count > 1), cf % cv].compact.join " "
        els.each{|el| bitmap[el[0]][el[1]] = "o"}
        bitmap.each{|row| puts row.gsub(/./, '\& ')}
        els.each{|el| bitmap[el[0]][el[1]] = "@"}
        gets
      end
    end
    return
  end
  
  dr2 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| (cx - cy).abs ** 0.5}.sum}
  d1 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| (cx - cy).abs}.sum}
  d2 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| (cx - cy) ** 2}.sum ** 0.5}
  d16 = -> x, y {x[2..4].zip(y[2..4]).map{|cx, cy| 
    high, low = (cx - cy).abs.divmod(16)
    2 * high + [low, 18 - low].min
  }.sum}

  # nearest neighbor heuristic from top left corner
  todo = plan
  plan = [todo.first]
  todo.shift
  until todo.empty?
    el = todo.min_by{|el| dr2[plan.last, el]}
    plan << el
    todo.delete el
  end

  # 2.5-opt: flip strands and move individual nodes
  # 3-opt: any three cuts
  [dr2].each do |d|
    loop do
      puts "score: #{plan.each_cons(2).map{|e1, e2| d[e1, e2]}.sum}"
      prev_plan = plan.dup
      (0 .. plan.length - 2).reverse_each do |lix|
        (lix + 2 .. plan.length).each do |rix|
          dol = lix == 0 ? 0 : d[plan[lix - 1], plan[lix]]
          dor = rix == plan.length ? 0 : d[plan[rix - 1], plan[rix]]
          dnl = lix == 0 ? 0 : d[plan[lix - 1], plan[rix - 1]]
          dnr = rix == plan.length ? 0 : d[plan[lix], plan[rix]]
          if dol + dor > dnl + dnr + 1e-9
            plan = plan[0 ... lix] + plan[lix ... rix].reverse + plan[rix ...]
            puts "flipped [#{lix} ... #{rix}]; saved #{dol + dor - dnl - dnr} points"
          end
        end
      end
      (puts "---"; redo) if prev_plan != plan
      (0 ... plan.length).each do |elix|
        (0 .. plan.length).each do |gapix|
          next if (elix - gapix).abs < 2
          dol = elix == 0 ? 0 : d[plan[elix - 1], plan[elix]]
          dor = elix == plan.length - 1 ? 0 : d[plan[elix], plan[elix + 1]]
          dog = gapix == 0 || gapix == plan.length ? 0 : d[plan[gapix - 1], plan[gapix]]
          dnl = gapix == 0 ? 0 : d[plan[gapix - 1], plan[elix]]
          dnr = gapix == plan.length ? 0 : d[plan[elix], plan[gapix]]
          dng = elix == 0 || elix == plan.length - 1 ? 0 : d[plan[elix - 1], plan[elix + 1]]
          if dol + dor + dog > dnl + dnr + dng + 1e-9
            plan = plan.dup
            plan.insert(gapix, plan[elix])
            plan.delete_at(elix > gapix ? elix + 1 : elix)
            puts "moved [#{elix}] to #{gapix}; saved #{dol + dor + dog - dnl - dnr - dng} points"
          end
        end
      end
      (puts "---"; redo) if prev_plan != plan
      (0 .. plan.length).to_a.combination(3) do |i1, i2, i3|
        # single element move or simple flip
        next if i2 == i1 + 1 || i3 == i2 + 1
        # ---a b---c d---e f---
        d_ab = i1 == 0 ? 0 : d[plan[i1 - 1], plan[i1]]
        d_ac = i1 == 0 ? 0 : d[plan[i1 - 1], plan[i2 - 1]]
        d_ad = i1 == 0 ? 0 : d[plan[i1 - 1], plan[i2]]
        d_ae = i1 == 0 ? 0 : d[plan[i1 - 1], plan[i3 - 1]]
        d_bd = d[plan[i1], plan[i2]]
        d_be = d[plan[i1], plan[i3 - 1]]
        d_bf = i3 == plan.length ? 0 : d[plan[i1], plan[i3]]
        d_cd = d[plan[i2 - 1], plan[i2]]
        d_ce = d[plan[i2 - 1], plan[i3 - 1]]
        d_cf = i3 == plan.length ? 0 : d[plan[i2 - 1], plan[i3]]
        d_df = i3 == plan.length ? 0 : d[plan[i2], plan[i3]]
        d_ef = i3 == plan.length ? 0 : d[plan[i3 - 1], plan[i3]]
        
        d_abcdef = d_ab + d_cd + d_ef - 1e-9
        # abcedf = simple flip
        # acbdef = simple flip
        d_acbedf = d_ac + d_be + d_df
        d_adebcf = d_ad + d_be + d_cf
        d_adecbf = d_ad + d_ce + d_bf
        d_aedbcf = d_ae + d_bd + d_cf
        # aedcbf = simple flip
        best = [d_abcdef, d_acbedf, d_adebcf, d_adecbf, d_aedbcf].min
        next if d_abcdef == best
        plan = plan [... i1] + case best
        when d_acbedf
          puts "double flip at #{[i1, i2, i3]}; saved #{d_abcdef - best} points"
          plan[i1 ... i2].reverse + plan[i2 ... i3].reverse
        when d_adebcf
          puts "strand exchange at #{[i1, i2, i3]}; saved #{d_abcdef - best} points"
          plan[i2 ... i3] + plan[i1 ... i2]
        when d_adecbf
          puts "swap and flip second at #{[i1, i2, i3]}; saved #{d_abcdef - best} points"
          plan[i2 ... i3] + plan[i1 ... i2].reverse
        when d_aedbcf
          puts "swap and flip first at #{[i1, i2, i3]}; saved #{d_abcdef - best} points"
          plan[i2 ... i3].reverse + plan[i1 ... i2]
        end + plan[i3 ...]
      end
      (puts "---"; redo) if prev_plan != plan
      break
    end
  end

  # display the results
  bitmap = xs.map{|x| " " * x.size}
  plan.each{|i, j, _, _, _| bitmap[i][j] = grid && (i % grid == grid - 2 || j % grid == grid - 2) ? "," : "."}
  ([[[nil, nil, nil], nil]] + plan.chunk{|el| el[2..4]}.to_a).each_cons(2) do |(prev_rgb, _), (rgb, els)|
    print "#{els.count}x " if els.count > 1
    puts "[#{rgb.zip(prev_rgb).map do |c, pc|
      "\e[3#{[7, 6, 3][pc.nil? ? 0 : c <=> pc]}m#{c}\e[0m"
    end.join ", "}]"
    els.each{|el| bitmap[el[0]][el[1]] = "o"}
    bitmap.each{|row| puts row.gsub(/./, '\& ')}
    els.each{|el| bitmap[el[0]][el[1]] = "@"}
    gets
  end
end

def bar(x, n: :n4, f: 0.1)
  r = relax_rescale_eigen(x, n:).lazy.select{_1[:delta_1] > 1e-10}.take(4).to_a
  c = r.each_cons(2).map{|r1, r2| (r1[:delta_1] / r2[:delta_1] * 256).floor.clamp(0..255) rescue 0}
  puts "ratio color = " + c.map{_1.to_s.rjust(3, "0")}.join(" ")
  puts "  \e[48;2;#{c.map(&:to_s).join(";")}m  \e[107;1m  \e[0m"
  
  sa = Hash[[0, 1, 2, 3].combination(2).map do |i, j|
    saij = r[i][:mode].flatten.compact.zip(r[j][:mode].flatten.compact).any? do |ei, ej|
      ei.abs > 1e-10 && ej.abs > 1e-10
    end
    [[i, j], saij]
  end]
  r0lone = !sa[[0, 1]] && !sa[[0, 2]] && !sa[[0, 3]]
  r1lone = !sa[[0, 1]] && !sa[[1, 2]] && !sa[[1, 3]]
  r2lone = !sa[[0, 2]] && !sa[[1, 2]] && !sa[[2, 3]]
  r3lone = !sa[[0, 3]] && !sa[[1, 3]] && !sa[[2, 3]]
  sag = case
        when r0lone && r1lone && r2lone && r3lone then 14
        when r0lone || r1lone || r2lone || r3lone
          (r0lone ? 1 : 0) + (r1lone ? 2 : 0) + (r2lone ? 4 : 0) + (r3lone ? 8 : 0)
        when !sa[[0, 2]] && !sa[[0, 3]] && !sa[[1, 2]] && !sa[[1, 3]] then 13
        when !sa[[0, 1]] && !sa[[0, 3]] && !sa[[1, 2]] && !sa[[2, 3]] then 11
        when !sa[[0, 1]] && !sa[[0, 2]] && !sa[[1, 3]] && !sa[[2, 3]] then 7
        else 0
        end

  sag_id = [0, 4, 3, 13, 2, 11, 12, 5, 1, 10, 9, 6, 8, 7, 14]
  sag_name = %w{aaaa aaa_ aa_a aa__ a_aa a_a_ a__a abba _aaa _aa_ _a_a baba __aa bbaa ____}
  puts "same area group id #{sag_id[sag]} = #{sag_name[sag]}"
end

def generate_palette n_colors, adjacencies
  # using redmean from https://en.m.wikipedia.org/wiki/Color_difference#sRGB
  # caused unstability. Switched to constant weights instead.
  c = n_colors.times.map{[rand - 0.5, rand - 0.5, rand - 0.5]}
  (1 .. 8092).each do |t|
    deltas = c.map{[0, 0, 0]}
    (0 ... n_colors - 1).each do |i|
      (i + 1 ... n_colors).each do |j|
        r = (c[i][0] + c[j][0] + 2) / 4
        sq_dist = (c[i][0] - c[j][0]) ** 2 +
                  1.6 * (c[i][1] - c[j][1]) ** 2 +
                  (c[i][2] - c[j][2]) ** 2
        deltas[i][0] += (c[i][0] - c[j][0])/sq_dist
        deltas[i][1] += (c[i][1] - c[j][1])/sq_dist
        deltas[i][2] += (c[i][2] - c[j][2])/sq_dist
        deltas[j][0] += (c[j][0] - c[i][0])/sq_dist
        deltas[j][1] += (c[j][1] - c[i][1])/sq_dist
        deltas[j][2] += (c[j][2] - c[i][2])/sq_dist
      end
    end
    adjacencies.each do |i, j|
      diff_c = c[i][0] - c[j][0]; deltas[i][0] -= diff_c / 2; deltas[j][0] += diff_c / 2
      diff_c = c[i][1] - c[j][1]; deltas[i][1] -= diff_c / 2; deltas[j][1] += diff_c / 2
      diff_c = c[i][2] - c[j][2]; deltas[i][2] -= diff_c / 2; deltas[j][2] += diff_c / 2
    end
    max_c = 0
    (0 ... n_colors).each do |i|
      c[i][0] += deltas[i][0]
      c[i][1] += deltas[i][1]
      c[i][2] += deltas[i][2]
    end
    min_c, max_c = [-1, 1, *c.map{_1[0]}].minmax
    mid_c = (min_c + max_c) / 2
    c.each{_1[0] = (_1[0] - mid_c) / (max_c - mid_c)}
    min_c, max_c = [-1, 1, *c.map{_1[1]}].minmax
    mid_c = (min_c + max_c) / 2
    c.each{_1[1] = (_1[1] - mid_c) / (max_c - mid_c)}
    min_c, max_c = [-1, 1, *c.map{_1[2]}].minmax
    mid_c = (min_c + max_c) / 2
    c.each{_1[2] = (_1[2] - mid_c) / (max_c - mid_c)}
    c.each{print "\e[48;2;%d;%d;%dm  \e[0m" % _1.map{|x| (127.5 * (x + 1)).round}}
    print "\n\e[A"
    puts if t & t - 1 == 0
  end
  puts
  c.each{print "#%02x%02x%02x;" % _1.map{|x| (127.5 * (x + 1)).round}}
  nil
rescue Interrupt, IRB::Abort
  puts
  c.each{print "#%02x%02x%02x;" % _1.map{|x| (127.5 * (x + 1)).round}}
  nil
end

def coinflip_race(n_trials, n_racers, min_ahead, p_move)
  result_tally = {}
  n_trials.times do
    positions = [0] * n_racers
    (0 ..).each do |t|
      positions.each_index{|i| positions[i] += 1 if rand < p_move}
      min, max = positions.minmax
      if max - min >= min_ahead
        result_tally[t] ||= 0
        result_tally[p t] += 1
        break
      end
      p [min, max, max - min, t] if t % 10000 == 0
    end
  end
  result_tally.to_a.sort
end

def gen_polyomino(n_tiles)
  tiles = [[0, 0]]
  neigh = [[-1, 0], [0, -1], [0, 1], [1, 0]]
  tiles |= [tiles.sample.zip(neigh.sample).map{_1+_2}] until tiles.length == n_tiles
  bb = tiles.transpose.map(&:minmax)
  (bb[0][0] .. bb[0][1]).map do |y|
    (bb[1][0] .. bb[1][1]).map do |x|
      tiles.include?([y, x]) ? "#" : " "
    end.join(" ")
  end.join("\n")
end

class Numeric
  def round_toward(other); round(half: (self > other) ^ (self < 0) ? :down : :up); end
  def round_away(other); round(half: (self < other) ^ (self < 0) ? :down : :up); end
end

class Rational
  def to_mixed_s
    if denominator == 1
      "#{self < 0 ? ?- : ""}#{to_i.abs}r"
    else
      "#{self < 0 ? ?- : ""}#{to_i.abs} #{(self-to_i).numerator.abs}/#{denominator}"
    end
  end
  def gcd(other); self.numerator.gcd(other.numerator).to_r / self.denominator.lcm(other.denominator); end
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

# math yanked from https://en.m.wikipedia.org/wiki/Delaunay_triangulation 
# and https://en.m.wikipedia.org/wiki/Circumscribed_circle#Triangles
class Triangle
  def initialize(*pts, z_metric:)
    @pts = pts.map{|pt| Vector[pt[0], pt[1], 0]}
    @es = [@pts[2] - @pts[1], @pts[0] - @pts[2],  @pts[1] - @pts[0]]
    @pts_paraboloid = @pts.map{|pt| Vector[pt[0], pt[1], pt.dot(pt)]}
    @ns = pts.map{|pt| pt[2]}
    z_gap = [
      z_metric[@ns[0], @ns[1]],
      z_metric[@ns[1], @ns[2]],
      z_metric[@ns[2], @ns[0]]
    ].max
    @tonedown = case
                when z_gap > z_metric[@ns[2], @ns[0]] && z_gap > z_metric[@ns[0], @ns[1]] then 0
                when z_gap > z_metric[@ns[0], @ns[1]] && z_gap > z_metric[@ns[1], @ns[2]] then 1
                when z_gap > z_metric[@ns[1], @ns[2]] && z_gap > z_metric[@ns[2], @ns[0]] then 2
                else nil
                end
    @priority = [ # not 1x1; maximize gradient; minimize perimeter; top to bottom
      (@es[0].dot(@es[0]) < 3 && @es[1].dot(@es[1]) < 3 && @es[2].dot(@es[2]) < 3) ? 0 : 1,
      z_gap,  
      (z_gap > 0 ? -1 : 1) * @es.map(&:norm).sum,
      pts.sort.flatten.map(&:-@)
    ]
    puts self
    (puts "triangle rejected"; raise ArgumentError) if @es[1].cross(@es[0])[2] <= 0
  end

  def pts; @pts.zip(@ns).map{|pt, n| [pt[0], pt[1], n]}; end
  def reject; @priority[0] = 0; end
  def rejected?; @priority[0] == 0; end
  def z_gap; @priority[1]; end
  attr_reader :priority

  def include?(x, y)
    pt = Vector[x, y, 0]
    (pt - @pts[0]).cross(@pts[1] - @pts[0])[2] >= 0 &&
    (pt - @pts[1]).cross(@pts[2] - @pts[1])[2] >= 0 &&
    (pt - @pts[2]).cross(@pts[0] - @pts[2])[2] >= 0
  end

  def circumcircle_include?(x, y)
    xyp = Vector[x, y, x**2 + y**2]
    Matrix::LUPDecomposition.new(Matrix.rows(@pts_paraboloid.map{|pt| (xyp - pt).to_a})).det >= 0
  end
  
  def centroid; (@pts.inject(&:+) / 3r).to_a; end

  def bounding_center
    pts = @pts.map{|pt| pt.map &:to_r}
    pt = case
         when @es[0].dot(@es[1]) >= 0 then (pts[0] + pts[1]) / 2
         when @es[1].dot(@es[2]) >= 0 then (pts[1] + pts[2]) / 2
         when @es[2].dot(@es[0]) >= 0 then (pts[2] + pts[0]) / 2
         else
            l0_2 = @es[0].dot @es[0]
            l1_2 = @es[1].dot @es[1]
            l2_2 = @es[2].dot @es[2]
            w0 = l0_2 * (l1_2 + l2_2 - l0_2)
            w1 = l1_2 * (l2_2 + l0_2 - l1_2)
            w2 = l2_2 * (l0_2 + l1_2 - l2_2)
            pt = (w0 * pts[0] + w1 * pts[1] + w2 * pts[2]) / (w0 + w1 + w2)
            gcd = @pts.flat_map{|pt2| [pt[0] - pt2[0], pt[1] - pt2[1]]}.reduce(0r, &:gcd)
            xys = @pts.map do |pt2|
              [0, 1].map{|ix| (pt[ix] - pt2[ix]).abs / gcd}.sort
            end.sort.uniq
            puts xys.map{|x, y| "#{x.to_mixed_s}^2 + #{y.to_mixed_s}^2"}.join(" = ")
                    .gsub(/0r\^2 + \| \+ 0r\^2/, "")
            pt
         end
    [pt[0], pt[1]]
  end

  def to_s
    "T[#{"\e[30;1m" if @tonedown == 0}#{pts[0]}\e[0m, #{
         "\e[30;1m" if @tonedown == 1}#{pts[1]}\e[0m, #{
         "\e[30;1m" if @tonedown == 2}#{pts[2]}\e[0m]"
  end
  def inspect; to_s; end
end
def voronoi_subdivide(xs, ys, reflexive = false, z_metric: -> a, b {(a - b).abs})

  p [xs.first, ys.first]
  n_0_0 = gets.to_i
  p [xs.last, ys.first] unless reflexive
  n_1_0 = gets.to_i unless reflexive
  p [xs.first, ys.last]
  n_0_1 = gets.to_i
  p [xs.last, ys.last]
  n_1_1 = gets.to_i
  xl = xs.length - 1
  yl = ys.length - 1
  triangles = [
    Triangle.new([0, 0, n_0_0], [0, yl, n_0_1], [xl, yl, n_1_1], z_metric:),
    (Triangle.new([xl, 0, n_1_0], [0, 0, n_0_0], [xl, yl, n_1_1], z_metric:) unless reflexive)
  ].compact
  lines_by_z_gap = Hash.new{|h, k| h[k] = []}
  
  loop do
    print "\npop "
    t = triangles.max_by(&:priority)
    return if t.rejected?
    x, y = nil
    if lines_by_z_gap.empty? || t.z_gap > lines_by_z_gap.keys.max
      p t
      cx, cy = t.bounding_center
      ox, oy = t.centroid
      x = cx.round_away ox
      y = cy.round_away oy
      op = "<-"
      if t.pts.any?{|px, py, _| px == x && py == y} || x > y && reflexive
        x = cx.round_toward ox
        y = cy.round_toward oy
        op = "->"
      end
      if triangles.any?{|t| t.pts.any?{|px, py, _| px == x && py == y}}
        t.reject
        puts "rejected"
        next
      end
      puts case
      when cx.denominator != 2 && cy.denominator != 2
        "[#{cx.to_mixed_s}, #{cy.to_mixed_s}] | [#{ox.to_mixed_s}, #{oy.to_mixed_s}]"
      when op == "->"
        "[#{cx.to_mixed_s}, #{cy.to_mixed_s}] -> [#{ox.to_mixed_s}, #{oy.to_mixed_s}]"
      else
        "<- [#{cx.to_mixed_s}, #{cy.to_mixed_s}] | [#{ox.to_mixed_s}, #{oy.to_mixed_s}]"
      end
    else
      line = p lines_by_z_gap[lines_by_z_gap.keys.max].min
      if (line[0][0] - line[1][0]) ** 2 + (line[0][1] - line[1][1]) ** 2 == 4
        x = (line[0][0] + line[1][0]) / 2
        y = (line[0][1] + line[1][1]) / 2
        x, y = nil if triangles.any?{|t| t.pts.any?{|px, py| px == x && py == y}}
      else
        x, y = ([line[0][0], line[1][0]].max - 1 .. [line[0][0], line[1][0]].min + 1).to_a
          .product(([line[0][1], line[1][1]].max - 1 .. [line[0][1], line[1][1]].min + 1).to_a)
          .find do |x, y|
            x >= 0 && x <= xl && y >= 0 && y <= yl && (x <= y || !reflexive) &&
              !triangles.any?{|t| t.pts.any?{|px, py| px == x && py == y}}
          end
      end
      if x.nil?
        lines_by_z_gap.each_value{|ls| ls.delete line}
        lines_by_z_gap.reject!{|_, v| v.empty?}
        next
      end
    end
    p x, y
    p [xs[x], ys[y]]

    n = nil
    loop do
      case gets
      when /^(\d+)$/ then n = $1.to_i
      when /^(\d+) (\d+) (\d+)$/
        x = $1.to_i; y = $2.to_i; n = $3.to_i
        lines_by_z_gap.each_value do |ls|
          ls.reject!{|pt1, pt2| pt1[0] == x && pt1[1] == y || pt2[0] == x && pt2[1] == y}
        end
        lines_by_z_gap.reject!{|_, v| v.empty?}
      else redo
      end
      break
    end

    ts = triangles.select{|t| t.circumcircle_include?(x, y)}
    triangles.reject!{|t| ts.include? t}
    ts.each{|t| puts ?- + t.to_s}
    pts = p ts.flat_map(&:pts).uniq.sort_by{|px, py, _| Math.atan2(px - x, py - y)}.reject{|px, py| px == x && py == y}
    triangles += (pts + [pts.first]).each_cons(2).map{|pt1, pt2| Triangle.new(pt1, pt2, [x, y, n], z_metric:) rescue nil}.compact
    pts.each do |pt|
      if (pt[0] - x).abs <= 2 && (pt[1] - y).abs <= 2
        lines_by_z_gap[z_metric[pt[2], n]] << p([pt, [x, y, n]].sort) 
      end
    end
  end
end
