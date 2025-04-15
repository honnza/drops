require "io/console"

Crate = Struct.new :ascii, :id, :pos do
  def self.alpha2(id, pos = nil); new((?A..?Z).sample + (?a..?z).sample, id, pos); end
  def self.rgba2(id, pos = nil)
    new("\e[38;2;#{rand 155 .. 255};#{rand 155 .. 255};#{rand 155 .. 255}m" +
        "\e[48;2;#{rand 0..100};#{rand 0..100};#{rand 0..100}m" +
        "#{(?A..?Z).to_a.sample}#{(?a..?z).to_a.sample}\e[0m", id, pos)
  end
  def self.rgbcjk(id, pos = nil)
    new("\e[38;2;#{rand 155 .. 255};#{rand 155 .. 255};#{rand 155 .. 255}m" +
        "\e[48;2;#{rand 0..100};#{rand 0..100};#{rand 0..100}m" +
        "#{rand(0x4e00 .. 0x9fef).chr(Encoding::UTF_8)}\e[0m", id, pos)
  end
  def self.gradient_alpha2(id, gradient, pos = nil)
    r, g, b = [0, 2/3r, 4/3r]
      .map{Math.cos(-_1 * Math::PI + 2 * gradient)}
      .map{((1 + _1) / 2 ** 0.5 * 256).floor.clamp(0..255)}

    new("\e[30;48;2;#{r};#{g};#{rand 0..255}m" +
        "#{(?A..?Z).to_a.sample}#{(?a..?z).to_a.sample}\e[0m", id, pos)
  end
end

class Layout

  # grid depicting the layout: 
  #   # are walls. ^>v< denote the path from each internal point to the exit(s).
  attr_reader :flow_map
  def initialize(flow_map); @flow_map = flow_map; end
  def width; @flow_map[0].length; end
  def height; @flow_map.length; end

  # coordinates of every place that a crate can be
  def places
    flow_map.flat_map.with_index do |row, ri|
      row.chars.map.with_index{|cell, ci| [ri, ci] if "^>v<"[cell]}.compact
    end
  end

  # the last place that a crate stored at that location passes through. This may point into a wall.
  # Returns nil if a wall coordinate is provided.
  def prev((ri, ci))
    case flow_map[ri][ci]
    when '^' then [ri - 1, ci]
    when '>' then [ri, ci + 1]
    when 'v' then [ri + 1, ci]
    when '<' then [ri, ci - 1]
    end
  end

  # calculates the list of crate spots that don't lie on the path of another crate
  def leaves; p = places; p - p.map{prev _1}; end
  # calculates the bits of wall that crates pass through
  def exits; @exits_cache ||= (p = places; p.map{prev _1}.uniq - p); end

  # calculates the places a crate needs to pass through to exit the warehouse.
  # Includes both the exit wall and the place the current spot.
  def path(pt)
    r = [pt]
    loop do
      pt = prev pt
      return r if pt.nil?
      r << pt
    end
  end

  # calculates the distance of a crate spot from a warehouse exit. Depth of 1 is the exit itself.
  def depth(pt); path(pt).length; end

  # calculates how many crate exit paths pass through this location
  def subtree_size(pt);
    unless @subtree_size_cache
      @subtree_size_cache = {}
      places.each do |place|
        path(place).each do |pos|
          @subtree_size_cache[pos] ||= 0
          @subtree_size_cache[pos] += 1
        end
      end
    end
    @subtree_size_cache[pt]
  end

  # calculates the amount of space within the warehouse,
  # including space reserved to extract crates through.
  def area; places.count; end
  # calculates how many crates can be stored within the warehouse. The longest path to a crate
  # is reserved to extract crates, the rest of the inner area is usable.
  def capacity; area - places.map{depth _1}.max + 2; end

  def render_cells(r_range, c_range)
    r = @flow_map[r_range].map do |row|
      row[c_range].chars.map{|cell| cell == "#" ? "\e[47m  \e[0m" : ".."}
    end
    exits.each do |ri, ci|
      next unless r_range.include?(ri) && c_range.include?(ci)
      r[ri - r_range.first][ci - c_range.first] = "\e[41m  \e[0m"
    end
    r
  end

  def as_maze
    (0 .. height - 2).map do |ri|
      (0 .. width - 2).map do |ci|
        ul = @flow_map[ri][ci]
        ur = @flow_map[ri][ci + 1]
        bl = @flow_map[ri + 1][ci]
        hz = (ul == ?v || bl == ?^ || ul == ?# && bl == ?#) ? " " : "_"
        vt = (ul == ?> || ur == ?< || ul == ?# && ur == ?#) ? "," : "|"
        hz + vt
      end.join
    end
  end

  def as_leaf_flow
    (0 .. height - 1).map do |ri|
      (0 .. width - 1).map do |ci|
        case
        when @flow_map[ri][ci] == ?# then "\e[47m  \e[0m"
        when subtree_size([ri, ci]) == 1 then "\e[100m#{@flow_map[ri][ci] * 2}\e[0m"
        else @flow_map[ri][ci] * 2
        end
      end.join
    end
  end


  # modifies a layout such that a crate's path doesn't touch itself orthogonally
  def chordless
    flow_map = @flow_map.map(&:dup)
    places.each do |ri, ci|
      rj, cj = path([ri, ci]).reverse_each.find do |rj, cj|
        (ri - rj).abs + (ci - cj).abs == 1
      end
      flow_map[ri][ci] = case [rj, cj]
                         when [ri - 1, ci] then ?^
                         when [ri, ci + 1] then ?>
                         when [ri + 1, ci] then ?v
                         when [ri, ci - 1] then ?<
                         end
    end
    Layout.new flow_map
  end

  def frain
    Layout.frain(width, height){|ri, ci| subtree_size([ri, ci])}
  end

  # modifies a layout such that a crate always exits towards the neighbor
  # through which most other crates already pass. Implies chordless
  def tight
    def compress(str)
      str = str.gsub(/(?<= |^)(.)\d+(?: \1\d+)+(?=[ .])/) do
        "#{$1}#{$&.scan(/\d+/).join("/")}"
      end
      str = str.gsub(/(?<= |^)(.+?)((?: \1)+)(?=[ .])/) do
        mantissa = $1.include?(" ") ? "(#{$1})" : $1
        exponent = $2.length / ($1.length + 1) + 1
        r = "#{exponent}#{mantissa}"
        r.length < $&.length ? r : $&
      end
      str[/^(... )?\S+/] = "..." while str.length > IO.console.winsize[1]
      str
    end

    def pif flow_map, text_line
      IO.console.cursor = [0, 0]
      if IO.console.winsize[1] / 2 >= flow_map[0].length
        puts Layout.new(flow_map).as_leaf_flow
      end
      IO.console.erase_line 2
      print compress(text_line)
    end

    flow_map = @flow_map
    history = []
    text_line = ""
    loop do
      old_layout = Layout.new(flow_map.map(&:dup))
      history << old_layout.flow_map

      # In the first ("append") phase, we reconnect smaller subtrees to larger subtrees. This cannot
      # form loops, because a node's subtree cannot be bigger than the subtree that node is in.
      places.each do |ri, ci|
        if depth([ri, ci]) > 2
          flow_map[ri][ci] = [
            [old_layout.subtree_size([ri - 1, ci]), old_layout.depth([ri - 1, ci]), rand, ?^],
            [old_layout.subtree_size([ri, ci + 1]), old_layout.depth([ri, ci + 1]), rand, ?>],
            [old_layout.subtree_size([ri + 1, ci]), old_layout.depth([ri + 1, ci]), rand, ?v],
            [old_layout.subtree_size([ri, ci - 1]), old_layout.depth([ri, ci - 1]), rand, ?<],
          ].reject{_1.first.nil?}.max.last
        end
      end

      diff = flow_map.each_index.flat_map do |ri|
        flow_map[ri].chars.each_index.filter_map do |ci|
          [ri, ci] unless flow_map[ri][ci] == old_layout.flow_map[ri][ci]
        end
      end
      if diff.length > 0
        if history.include?(flow_map)
          ri, ci = diff.sample
          new_flow_map = old_layout.flow_map.map(&:dup)
          new_flow_map[ri][ci] = flow_map[ri][ci]
          flow_map = new_flow_map
          text_line << "AS "
        else
          text_line << "A#{diff.length} "
        end
        pif flow_map, text_line
        next
      end

      # In the second ("bump") phase, we allow reconnecting subtrees to smaller subtrees if they
      # become bigger than the current predecessor after reconnection, but only if the new path
      # isn't longer than the current one.
      p_coordinator = Hash.new{|h, k| h[k] = []}
      n_coordinator = Hash.new{|h, k| h[k] = []}
      places.each do |ri, ci|
        dir = flow_map[ri][ci]
        if depth([ri, ci]) > 2
          candidates = []
          [
            [ri - 1, ci, ?^], [ri, ci + 1, ?>], [ri + 1, ci, ?v], [ri, ci - 1, ?<]
          ].each do |rj, cj, djr|
            score = old_layout.subtree_size([rj, cj])
            next if score.nil?
            score -= old_layout.subtree_size([ri, ci]) if dir == djr
            if old_layout.depth([rj, cj]) < old_layout.depth([ri, ci])
              candidates << [score, dir == djr ? 0 : 1, rand, rj, cj, djr]
            end
          end
          current = candidates.find{_1[5] == dir}
          best = candidates.max
          if (current[0] <=> best[0]) < 0
            flow_map[ri][ci] = best[5]
          elsif current != best
            candidates.sort.reverse.take_while{_1 != current}
                      .each{p_coordinator[_1[3, 2]] << [ri, ci, _1[5]]}
            n_coordinator[current[3, 2]] << [ri, ci, best[5]]
          end
        end
      end

      diff = flow_map.each_index.flat_map do |ri|
        flow_map[ri].chars.each_index.filter_map do |ci|
          [ri, ci] unless flow_map[ri][ci] == old_layout.flow_map[ri][ci]
        end
      end
      if diff.length > 0
        if history.include?(flow_map)
          ri, ci = diff.sample
          new_flow_map = old_layout.flow_map.map(&:dup)
          new_flow_map[ri][ci] = flow_map[ri][ci]
          flow_map = new_flow_map
          text_line << "BS "
        else
          text_line << "B#{diff.length} "
        end
        pif flow_map, text_line
        next
      end

      # finally, if the bump phase doesn't do anything either, we coordinate (C) bumps that would
      # have been neutral by themselves but are beneficial in tandem.

      p_coordinator.reject!{|_, v| v.length < 2}
      n_coordinator.reject!{|_, v| v.length < 2}
      unless p_coordinator.empty? & n_coordinator.empty?
        p_diff = p_coordinator.values.flatten(1)
        n_diff = n_coordinator.values.flatten(1)
        if n_diff.empty?
          text_line << "C#{p_diff.length} "
        else
          text_line << "C#{p_diff.length}+#{n_diff.length} "
        end
        (p_diff + n_diff).shuffle.each{|ri, ci, dir| flow_map[ri][ci] = dir}
        pif flow_map, text_line
        next
      end

      break
    end
    text_line[-1] = "."
    pif flow_map, text_line
    Layout.new(flow_map)
  end

  class << self
    # creates a rectangular warehouse with empty lines every third line,
    # and one perpendicular line connecting them to the exit. Width and height include the border.
    # Orientation is either horizontal (parallel lines run left to right), vertical
    # (parallel lines run top to bottom), or auto (chosen to maximize the number of leaf nodes).
    def regular_3(w, h, orientation = :auto)
      # the leaf count is the area minus the number of parallel lanes times their length - 1,
      # plus the size perpendicular to lane orientation - 1.
      # The final - 1 doesn't apply if the inner size mod 3 is 1 (far lane is at the edge).
      # The number of lanes is the perpendicular size divided by 3.
      #
      # ########### ########### ###########
      # #>>>>^<<<<# #vvv>^<vvv# #>>>>^<<<<#
      # #^^^^^^^^^# #>>>>^<<<<# #^^^^^^^^^#
      # ########### #^^^^^^^^^# #vvv>^<vvv#
      #             ########### #>>>>^<<<<#
      #                         ###########

      uncap_hz = (h / 3) * (w - 3) + h - (h % 3 == 0 ? 0 : 1)
      uncap_vt = (w / 3) * (h - 3) + w - (w % 3 == 0 ? 0 : 1)

      if orientation == :horizontal || orientation != :vertical && uncap_hz <= uncap_vt
        lane_at = h % 3 == 2 ? 2 : 1
        mid_at = w / 2
        Layout.new((0 ... h).map do |ri|
          (0 ... w).map do |ci|
            ri_rel = (ri - lane_at) % 3
            case
            when ri == 0 || ri == h - 1 || ci == 0 || ci == w - 1 then ?#
            when ci == mid_at || ri_rel == 1 then ?^
            when ri_rel == 2 then ?v
            when ci < mid_at then ?>
            else ?<
            end
          end.join
        end)
      else
        lane_at = w % 3 == 2 ? 2 : 1
        mid_at = h / 2
        Layout.new((0 ... h).map do |ri|
          (0 ... w).map do |ci|
            ci_rel = (ci - lane_at) % 3
            case
            when ri == 0 || ri == h - 1 || ci == 0 || ci == w - 1 then ?#
            when ri == mid_at || ci_rel == 1 then ?<
            when ci_rel == 2 then ?>
            when ri < mid_at then ?v
            else ?^
            end
          end.join
        end)
      end
    end

    # expands a tree of paths randomly to fill the inner area without regards
    # for the resulting capacity
    def prim(w, h)
      flow_map = [?# * w, *(h - 2).times.map{?# + ?. * (w - 2) + ?#}, ?# * w]
      open_set = [[
        *[*(1 .. w - 2)].map{[1, _1, ?^]},
        *[*(1 .. w - 2)].map{[h - 2, _1, ?v]},
        *[*(1 .. h - 2)].map{[_1, 1, ?<]},
        *[*(1 .. h - 2)].map{[_1, w - 2, ?>]}
      ].sample]
      until open_set.empty?
        ri, ci, dir = open_set.sample
        flow_map[ri][ci] = dir
        open_set.reject!{|rj, cj, _| ri == rj && ci == cj}
        open_set << [ri - 1, ci, ?v] if flow_map[ri - 1][ci] == ?.
        open_set << [ri, ci + 1, ?<] if flow_map[ri][ci + 1] == ?.
        open_set << [ri + 1, ci, ?^] if flow_map[ri + 1][ci] == ?.
        open_set << [ri, ci - 1, ?>] if flow_map[ri][ci - 1] == ?.
      end
      Layout.new flow_map
    end

    # expands a tree of paths randomly to fill the inner area without regards
    # for the resulting capacity, using a single value generated for each edge
    def pruskal(w, h, &rng)
      rng ||= -> _, _{rand}
      flow_map = [?# * w, *(h - 2).times.map{?# + ?. * (w - 2) + ?#}, ?# * w]
      open_set = [[
        *[*(1 .. w - 2)].map{[0, 1, _1, ?^]},
        *[*(1 .. w - 2)].map{[0, h - 2, _1, ?v]},
        *[*(1 .. h - 2)].map{[0, _1, 1, ?<]},
        *[*(1 .. h - 2)].map{[0, _1, w - 2, ?>]}
      ].min_by{|_, ri, ci, _| rng[nil, [ri, ci]]}]
      until open_set.empty?
        _, ri, ci, dir = open_set.min
        flow_map[ri][ci] = dir
        open_set.reject!{|_, rj, cj, _| ri == rj && ci == cj}
        open_set << [rng[[ri, ci], [ri - 1, ci]], ri - 1, ci, ?v] if flow_map[ri - 1][ci] == ?.
        open_set << [rng[[ri, ci], [ri, ci + 1]], ri, ci + 1, ?<] if flow_map[ri][ci + 1] == ?.
        open_set << [rng[[ri, ci], [ri + 1, ci]], ri + 1, ci, ?^] if flow_map[ri + 1][ci] == ?.
        open_set << [rng[[ri, ci], [ri, ci - 1]], ri, ci - 1, ?>] if flow_map[ri][ci - 1] == ?.
      end
      Layout.new flow_map
    end

    # expands a tree of paths randomly to fill the inner area without regards
    # for the resulting capacity, using a single value generated for each node
    def nuskal(w, h)
      rng = Hash.new{|h, k| h[k] = rand}
      pruskal(w, h){|i, j| [rng[i], rng[j]].sort}
    end

    def xyskal(w, h)
      r_rng = Hash.new{|h, k| h[k] = rand - 0.5}
      c_rng = Hash.new{|h, k| h[k] = rand - 0.5}

      pruskal(w, h) do |(ri, ci), (rj, cj)|
        r_rng[ri] ** 3 + c_rng[ci] ** 3 + r_rng[rj] ** 3 + c_rng[cj] ** 3
      end
    end

    # diagonal binary tree, rooted at the top left corner of the warehouse
    def dbt(w, h)
      Layout.new((0 ... h).map do |ri|
        (0 ... w).map do |ci|
          case
          when ri == 0 || ri == h - 1 || ci == 0 || ci == w - 1 then ?#
          when ri == 1 then ?<
          when ci == 1 then ?^
          else
            rix = ri - 1
            cix = ci - 1
            while rix % 2 == 0 && cix % 2 == 0
              rix /= 2
              cix /= 2
            end
            rix % 2 == 0 ? ?< : ?^
          end
        end.join
      end)
    end

    # randomized diagonal binary tree, rooted at the top left corner of the warehouse
    def rdbt(w, h)
      rng = (2 ... w - 1).map{[:c, _1, ?^]} +
            (2 ... h - 1).map{[:r, _1, ?<]}
      rng = [[:r, 1, ?<], [:c, 1, ?^]] + rng.shuffle
      Layout.new((0 ... h).map do |ri|
        (0 ... w).map do |ci|
          if ri == 0 || ri == h - 1 || ci == 0 || ci == w - 1
            ?#
          else
            rng.find{|rc, i, _| rc == :r && i == ri || rc == :c && i == ci}.last
          end
        end.join
      end)
    end

    # short for leaf rain - randomly marks places as leaf places such that every place is connected
    # to the exit.through non-leaf places.
    def frain(w, h, &rng)
      rng ||= ->_{0}
      leaf_map = h.times.map{[false] * w}
      [*1...h-1].product([*1...w-1]).sort_by{[rng[_1], rand]}.each do |ri, ci|
        leaf_map[ri][ci] = true
        flow_map = [?# * w, *(h - 2).times.map{?# + ?. * (w - 2) + ?#}, ?# * w]
        neighbors = [[ri - 1, ci], [ri, ci + 1], [ri + 1, ci], [ri, ci - 1]]
                      .reject{|ri, ci| flow_map[ri][ci] == ?#}
        bfs = [neighbors.find{|ri, ci| !leaf_map[ri][ci]} + [?x]]
        bfs.each do |rj, cj, djr|
          next if flow_map[rj][cj] != ?.
          flow_map[rj][cj] = djr
          next if leaf_map[rj][cj]
          bfs.push *[
            [rj - 1, cj, ?v],
            [rj, cj + 1, ?<],
            [rj + 1, cj, ?^],
            [rj, cj - 1, ?>]
          ].shuffle
          break unless neighbors.any?{|ri, ci| flow_map[ri][ci] == ?.}
        end
        if neighbors.any?{|ri, ci| flow_map[ri][ci] == ?.}
          leaf_map[ri][ci] = false
        else
          IO.console.cursor = [ri, ci * 2]
          puts "[]"
        end
      end
      
      candidates = [
        *[*(1 .. w - 2)].map{[1, _1, ?^]},
        *[*(1 .. w - 2)].map{[h - 2, _1, ?v]},
        *[*(1 .. h - 2)].map{[_1, 1, ?<]},
        *[*(1 .. h - 2)].map{[_1, w - 2, ?>]}
      ].map do |ri, ci, dir|
        flow_map = [?# * w, *(h - 2).times.map{?# + ?. * (w - 2) + ?#}, ?# * w]
        score = 0
        bfs = [[ri, ci, dir, 0]]
        bfs.each do |rj, cj, djr, depth|
          next if flow_map[rj][cj] != ?.
          flow_map[rj][cj] = djr
          score += depth
          next if leaf_map[rj][cj] && depth > 0
          bfs.push *[
            [rj - 1, cj, ?v, depth + 1],
            [rj, cj + 1, ?<, depth + 1],
            [rj + 1, cj, ?^, depth + 1],
            [rj, cj - 1, ?>, depth + 1]
          ].shuffle
        end
        layout = Layout.new flow_map
        [score, rand, ri, ci, dir, layout]
      end
      min, max = candidates.map(&:first).minmax
      candidates.each do |score, _, ri, ci, dir, _|
        IO.console.cursor = [ri, ci * 2]
        score = (score - min).fdiv(max - min)
        r, g = [score, 1 - score].map{(_1 ** 0.5 * 256).floor.clamp(0 .. 255)}
        c = leaf_map[ri][ci] ? "[]" : dir * 2
        print "\e[38;2;#{r};#{score == 0 ? 127 : g};#{score == 0 ? 255 : 0}m#{c}\e[0m"
      end
      STDIN.gets
      candidates.min.last
    end

    def xyfrain(w, h)
      r_rng = Hash.new{|h, k| h[k] = rand - 0.5}
      c_rng = Hash.new{|h, k| h[k] = rand - 0.5}

      frain(w, h) do |ri, ci|
        r_rng[ri] ** 3 + c_rng[ci] ** 3
      end
    end

    def manhattan(w, h)
      row_period = [rand(4 .. h / 2), rand(4 .. h / 2)].min
      row_offset = rand(0 ... row_period)
      road_rows = (1 .. h - 2).select{_1 % row_period == row_offset}
      middle_row = road_rows.sample

      col_period = [rand(4 .. w / 2), rand(4 .. w/ 2)].min
      col_offset = rand(0 ... col_period)
      road_cols = (1 .. w - 2).select{_1 % col_period == col_offset}
      middle_col = road_cols.sample

      IO.console.cursor = [middle_row, 2 * middle_col]
      puts "><"
      frain(w, h) do |ri, ci|
        if road_rows.include?(ri) || road_cols.include?(ci)
          [1, -(ri - middle_row).abs - (ci - middle_col).abs]
        else
          [0]
        end
      end
      end
  end
end

class Model
  def initialize(layout, crates = [])
    @layout = layout
    @crates = {}
    @worker_pos = nil
    crates.each {@crates[_1.id] = _1}
  end

  attr_reader :layout, :crates
  attr_accessor :worker_pos


  # lists the places not occupied by a crate
  def free_places; @layout.places - crates.values.map{_1.pos}; end

  # suggests an empty place. Places that block fewest other places are preferred,
  # then the closest one.
  def suggest_place
    (@layout.places - @crates.values.map(&:pos)).min_by do |pos|
      [@layout.subtree_size(pos), @layout.depth(pos), rand]
    end
  end

  def render(r_range = 0 ... @layout.height, c_range = 0 ... @layout.width)
    r = @layout.render_cells(r_range, c_range)
    @crates.each_value do |crate|
      ri, ci = crate.pos
      next unless r_range.include?(ri) && c_range.include?(ci)
      r[ri - r_range.first][ci - c_range.first] = crate.ascii
    end
    ri, ci = @worker_pos
    r[ri - r_range.first][ci - c_range.first] = "👷" unless ri.nil?
    r.map(&:join)
  end

  def render_frame(*diff)
    sleep_time = 0.03 - (Time.now - @t_prev_frame) if @t_prev_frame
    sleep sleep_time if sleep_time &.> 0
    @t_prev_frame = Time.now

    if diff.compact.empty?
      min_r = 0
      max_r = @layout.height - 1
      min_c = 0
      max_c = @layout.width - 1
    else
      min_r, max_r = diff.compact.map{_1[0]}.minmax
      min_c, max_c = diff.compact.map{_1[1]}.minmax
    end

    @viewport = nil if IO.console.winsize != @winsize

    vr, vc = @viewport
    if !(vr === min_r && vr === max_r && vc === min_c && vc === max_c)
      @winsize = IO.console.winsize
      vh = [@layout.height, @winsize[0]].min
      vr_start = ((min_r + max_r - vh) / 2).clamp(0 .. @layout.height - vh)
      vr = vr_start ... vr_start + vh

      vw = [@layout.width, @winsize[1] / 2].min
      vc_start = ((min_c + max_c - vw) / 2).clamp(0 .. @layout.width - vw)
      vc = vc_start ... vc_start + vw
      @viewport = [vr, vc]

      IO.console.clear_screen
      print render(vr, vc).join("\n")
    elsif diff.empty?
      IO.console.cursor = [0, 0]
      print render(vr, vc).join("\n")
    else
      diff.compact!
      render(min_r .. max_r, min_c .. max_c).each.with_index(min_r) do |row, ri|
        IO.console.cursor = [ri - vr.first, (min_c - vc.first) * 2]
        print row
      end
    end
  end

  # moves a crate along the specified path, with the worker following after it
  def animate_insert(crate, path)
    path.each_cons(2) do |p1, p2|
      p0 = @worker_pos
      @worker_pos = p1
      crate.pos = p2
      render_frame(p0, p1, p2)
    end
  end

  # moves the worker along the specified path
  def animate_worker(path)
    path.each do |p1|
      p0 = @worker_pos
      @worker_pos = p1
      render_frame(p0, p1)
    end
  end

  # moves a crate along the specified path, with the worker leading before it
  def animate_extract(crate, path)
    path.each_cons(2) do |p1, p2|
      p0 = crate.pos
      crate.pos = p1
      @worker_pos = p2
      render_frame(p0, p1, p2)
    end
  end
end

w, h = case ARGV.length
       when 1 then [IO.console.winsize[1] / 2, IO.console.winsize[0] - 1]
       when 2 then [ARGV[1].to_i, ARGV[1].to_i]
       when 3 then [ARGV[1].to_i, ARGV[2].to_i]
       else
         puts "1 .. 3 arguments expected"
         exit
       end

layouts = %i{horizontal vertical regular prim pruskal nuskal xyskal frain xyfrain dbt rdbt manhattan}
unless ARGV[0] =~ /^((?:frain-|chordless-|tight-)*)(#{layouts.join ?|})$/
    puts "first argument should be #{layouts.join ", "}, or frain-, tight- or chordless- plus one of the preceding"
  exit
end

begin
  puts "\e[?25l"
  IO.console.clear_screen

  layout = case $2
           when "horizontal" then Layout.regular_3 w, h, :horizontal
           when "vertical" then Layout.regular_3 w, h, :vertical
           when "regular" then Layout.regular_3 w, h, :auto
           else Layout.send $2, w, h
           end

  $1&.split("-")&.reverse_each do |lad|
    layout = layout.send lad
  end

  model = Model.new layout

  IO.console.cursor = [0, 0]
  puts model.layout.as_leaf_flow
  STDIN.gets

  cap = model.layout.capacity
  cap.times do |id|
    pos = model.suggest_place
    crate = Crate.gradient_alpha2 id, Math.log(layout.subtree_size(pos)) / Math.log(cap)
    model.crates[id] = crate
    path = model.layout.path pos
    model.animate_insert crate, [nil, nil] + path.reverse
    model.animate_worker path[2..]
  end
ensure
  IO.console.cursor = [h, 0]
  puts "\e[?25h"
end
