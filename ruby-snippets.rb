require "io/console"
require "matrix"
def gc x; (255 * x ** (1/2.2)).round; end

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

def progress_bar progress, text = "", width = IO.console.winsize[1] - 1
  on_cells = ((width - 2) * progress.clamp(0 .. 1))
  bg = (on_cells % 1 * 256).floor
  fg = bg > 127 ? 30 : 97
  (text.ljust(width - 2) + "]")
    .insert(on_cells.floor + 1, "\e[0m")
    .insert(on_cells.floor, "\e[48;2;#{bg};#{bg};#{bg};#{fg}m")
    .insert(0, "[\e[107;30m")
end

def relax_rescale(grid, f: 0.1, n: :n4, s: [])
  strength = f; neighborhood = n; suppressed_modes = s.dup
  grid = grid.split(/[\n\/]/).map{|row| row.chars.map{|c|
    {?- => -1.0, ?. => 0.0, ?+ => 1.0, ?? => rand(-1 .. 1), ?e => rand(-1e-10 .. 1e-10)}[c]
  }} if grid.is_a? String
  precision = IO.console.winsize[1] / grid.map(&:length).max - 1
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
      grid.each.with_index{|row, i| cout << row.map.with_index{|val, j| fmt[i, j, val]}.join(" ").rstrip}
      cout << ""
      cout << "@#{t}"
      cout << "energy = %.16f" % [energy]
      cout << "delta_1 = %.16f" % [(1 - scale) / strength]
      # cout << "suppression factors = %p" % [suppression_factors.map{|f| "%.1e" % f}]
      # \e[A moves cursor up; \e[?25l hides it; \e[?25h shows it again
      cout << progress_bar(
        Math.log(max_delta || 1) / Math.log(Float::EPSILON),
        "elapsed: #{fmt_secs[Time.now - time_start]} | remaining: #{fmt_secs[smooth_time_est]}"
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

def foo(x, limit = nil, filter: nil, n: :n4, f: 0.1, grid: nil, hicolor: false, rgb: false, png: false)
  # generate channels
  xs = x.split(/[\/\n]/)
  modes = []
  accepted_modes = []
  loop do
    modes << relax_rescale(x, n: n, s: modes.map{|x| x[:mode]}, f: f)
    break if modes.last.nil? || limit.is_a?(Float) && modes.last[:delta_1] >= limit

    dot = xs.zip(modes.last[:mode]).map do |cs, ms|
      cs.chars.zip(ms).map{|c, m|m.nil? ? 0 : m * {"-" => -1, "+" => 1, "." => 0, "?" => rand(-1 .. 1), "e" => 0}.fetch(c, c)}
    end.flatten.sum
    abs_dot = xs.zip(modes.last[:mode]).map do |cs, ms|
      cs.chars.zip(ms).map{|c, m|m.nil? ? 0 : m.abs * {"-" => 1, "+" => 1, "." => 0, "?" => rand(-1 .. 1), "e" => 0}.fetch(c, c)}
    end.flatten.sum
    if dot.abs > 1e-7
      puts "mode #{modes.size} strength = #{dot} / #{abs_dot} = #{dot / abs_dot}", "---"
      accepted_modes << [dot / abs_dot, modes.size, modes.last[:mode]]
    else
      puts "rejected mode #{modes.size}: dot product = #{dot}"
      end
    puts "strongest modes: #{accepted_modes.sort.reverse.map{|_, ix, _| ix}}" if limit
    #sleep [dot / len * 2, 1].max
    gets
    break if limit.nil? && accepted_modes.size == 3 || limit.is_a?(Integer) && modes.size >= limit
  rescue Interrupt, IRB::Abort
    puts "user abort"
    break
  end

  accepted_modes.sort.reverse.each{|strength, ix, _| p [strength, ix]} if limit
  #sleep 5 if limit
  
  # transpose into pixels
  g, r, b = (limit.nil? ? accepted_modes : accepted_modes.max(3)).map(&:last)
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

class Float
  def round_toward(other); round(half: (self > other) ^ (self < 0) ? :down : :up); end
  def round_away(other); round(half: (self < other) ^ (self < 0) ? :down : :up); end
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
  def initialize(*pts)
    @pts = pts.map{|pt| Vector[pt[0], pt[1], 0]}
    @es = [@pts[2] - @pts[1], @pts[0] - @pts[2],  @pts[1] - @pts[0]]
    @pts_paraboloid = @pts.map{|pt| Vector[pt[0], pt[1], pt.dot(pt)]}
    @ns = pts.map{|pt| pt[2]}
    @priority = [ # not 1x1; not obtuse; maximize uncertainty in value; maximize area
      (@es[0].dot(@es[0]) < 3 && @es[1].dot(@es[1]) < 3 && @es[2].dot(@es[2]) < 3) ? 0 : 1,
      # (@es[0].dot(@es[1]) > 0 || @es[1].dot(@es[2]) > 0 || @es[2].dot(@es[0]) > 0) ? 0 : 1,
      @ns.max - @ns.min,
      @es[1].cross(@es[0])[2]
    ]
    puts self
    (puts "triangle rejected"; raise ArgumentError) if @priority.last <= 0
  end

  def pts; @pts.zip(@ns).map{|pt, n| [pt[0], pt[1], n]}; end
  attr_reader :priority

  def circumcircle_contains(x, y)
    xyp = Vector[x, y, x**2 + y**2]
    Matrix::LUPDecomposition.new(Matrix.rows(@pts_paraboloid.map{|pt| (xyp - pt).to_a})).det > 0
  end
  
  def obtuse_pt
    case
    when @es[0].dot(@es[1]) >= 0 then pts[2]
    when @es[1].dot(@es[2]) >= 0 then pts[0]
    when @es[2].dot(@es[0]) >= 0 then pts[1]
    else nil
    end
  end

  def bounding_center
    pts = @pts.map{|pt| pt.map &:to_f}
    pt = case
         when @es[0].dot(@es[1]) > 0 then (pts[0] + pts[1]) / 2
         when @es[1].dot(@es[2]) > 0 then (pts[1] + pts[2]) / 2
         when @es[2].dot(@es[0]) > 0 then (pts[2] + pts[0]) / 2
         else
            l0_2 = @es[0].dot @es[0]
            l1_2 = @es[1].dot @es[1]
            l2_2 = @es[2].dot @es[2]
            w0 = l0_2 * (l1_2 + l2_2 - l0_2)
            w1 = l1_2 * (l2_2 + l0_2 - l1_2)
            w2 = l2_2 * (l0_2 + l1_2 - l2_2)
            (w0 * pts[0] + w1 * pts[1] + w2 * pts[2]) / (w0 + w1 + w2)
         end
    [pt[0], pt[1]]
  end

  def to_s; "Triangle #{pts}, priority #{@priority}"; end
  def inspect; to_s; end
end
def voronoi_subdivide(xs, ys, reflexive = false)

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
    Triangle.new([0, 0, n_0_0], [0, yl, n_0_1], [xl, yl, n_1_1]),
    (Triangle.new([xl, 0, n_1_0], [0, 0, n_0_0], [xl, yl, n_1_1]) unless reflexive)
  ].compact
  
  loop do
    print "\npop "
    t = p triangles.max_by(&:priority)
    x, y = nil
    cx, cy = p t.bounding_center
    if t.obtuse_pt
      ox, oy = t.obtuse_pt
      x = cx.round_toward ox
      y = cy.round_toward oy
      if x == ox && y == oy
        x = cx.round_away ox
        y = cy.round_away oy
      end
    else
      x = cx.round
      y = cy.round
    end
    p x, y
    ts = triangles.select{|t| t.circumcircle_contains(x, y)}
    triangles.reject!{|t| ts.include? t}
    ts.each{|t| puts ?- + t.to_s}
    pts = p ts.flat_map(&:pts).uniq.sort_by{|px, py, _| Math.atan2(px - x, py - y)}
    p [xs[x], ys[y]]
    n = loop{break Integer(gets) rescue print "? "}
    triangles += (pts + [pts.first]).each_cons(2).map{|pt1, pt2| Triangle.new pt1, pt2, [x, y, n] rescue nil}.compact
  end
end
