require "io/console"

Crate = Struct.new :ascii, :id, :pos do
  def self.alpha2(id, pos = nil); new((?A..?Z).sample + (?a..?z).sample, id, pos); end
  def self.rgba2(id, pos = nil)
    new("\e[38;2;#{rand 155 .. 255};#{rand 155 .. 255};#{rand 155 .. 255}m" +
        "\e[48;2;#{rand 0..100};#{rand 0..100};#{rand 0..100}m" +
        "#{(?A..?Z).to_a.sample}#{(?a..?z).to_a.sample}\e[0m", id, pos)
  end
end

class Layout

  # grid depicting the layout: 
  #   # are walls. ^>v< denote the path from each internal point to the exit(s).
  attr_reader :flow_map
  def initialize(flow_map); @flow_map = flow_map; end

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
  def exits; p = places; p.map{prev _1}.uniq - p; end

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

  # calculates the amount of space within the warehouse,
  # including space reserved to extract crates through.
  def area; places.count; end
  # calculates how many crates can be stored within the warehouse. The longest path to a crate
  # is reserved to extract crates, the rest of the inner area is usable.
  def capacity; area - places.map{depth _1}.max + 2; end

  def render_cells
    r = @flow_map.map{|row| row.chars.map{|cell| cell == "#" ? "\e[47m  \e[0m" : ".."}}
    exits.each{|ri, ci| r[ri][ci] = "\e[41m  \e[0m"}
    r
  end

  def as_maze
    h = @flow_map.length
    w = @flow_map[0].length
    (0 .. h - 2).map do |ri|
      (0 .. w - 2).map do |ci|
        ul = @flow_map[ri][ci]
        ur = @flow_map[ri][ci + 1]
        bl = @flow_map[ri + 1][ci]
        hz = (ul == ?v || bl == ?^ || ul == ?# && bl == ?#) ? " " : "_"
        vt = (ul == ?> || ur == ?< || ul == ?# && ur == ?#) ? "," : "|"
        hz + vt
      end.join
    end
  end

  # modifies a layout such that a crate's path doesn't touch itself orthogonally
  def chordless
    flow_map = @flow_map
    places.each do |pos|
      ri, ci = pos
      rj, cj = path(pos).reverse_each.find do |rj, cj|
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
    # for the resulting capacity
    def pruskal(w, h)
      flow_map = [?# * w, *(h - 2).times.map{?# + ?. * (w - 2) + ?#}, ?# * w]
      open_set = [[
        *[*(1 .. w - 2)].map{[0, 1, _1, ?^]},
        *[*(1 .. w - 2)].map{[0, h - 2, _1, ?v]},
        *[*(1 .. h - 2)].map{[0, _1, 1, ?<]},
        *[*(1 .. h - 2)].map{[0, _1, w - 2, ?>]}
      ].sample]
      until open_set.empty?
        _, ri, ci, dir = open_set.min
        flow_map[ri][ci] = dir
        open_set.reject!{|_, rj, cj, _| ri == rj && ci == cj}
        open_set << [rand, ri - 1, ci, ?v] if flow_map[ri - 1][ci] == ?.
        open_set << [rand, ri, ci + 1, ?<] if flow_map[ri][ci + 1] == ?.
        open_set << [rand, ri + 1, ci, ?^] if flow_map[ri + 1][ci] == ?.
        open_set << [rand, ri, ci - 1, ?>] if flow_map[ri][ci - 1] == ?.
      end
      Layout.new flow_map
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

  # chooses an empty space: the closest leaf if one is available, or the furthest non-leaf otherwise
  def suggest_place
    leaves = @layout.leaves - crates.values.map{_1.pos}
    if leaves.empty?
      free_places.max_by{@layout.depth(_1)}
    else
      leaves.min_by{@layout.depth(_1)}
    end
  end

  def render
    r = @layout.render_cells
    @crates.each_value do |crate|
      ri, ci = crate.pos
      next if ri.nil?
      r[ri][ci] = crate.ascii
    end
    ri, ci = @worker_pos
    r[ri][ci] = "👷" unless ri.nil?
    r.map(&:join).join "\n"
  end

  def render_frame; IO.console.cursor = [0, 0]; puts render; sleep 0.03; end

  # moves a crate along the specified path, with the worker following after it
  def animate_insert(crate, path)
    path.each_cons(2) do |p1, p2|
      @worker_pos = p1
      crate.pos = p2
      render_frame
    end
  end

  # moves the worker along the specified path
  def animate_worker(path)
    path.each do |pt|
      @worker_pos = pt
      render_frame
    end
  end

  # moves a crate along the specified path, with the worker leading before it
  def animate_extract(crate, path)
    path.each_cons(2) do |p1, p2|
      crate.pos = p1
      @worker_pos = p2
      render_frame
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

unless ARGV[0] =~ /^(chordless-)?(horizontal|vertical|regular|pruskal|prim|dbt)/
  puts "first argument should be horizontal, vertical, regular, dbt, prim, pruskal or chordless- plus one of the preceding"
end

layout = case ARGV[0].split("-").last
         when "horizontal" then Layout.regular_3 w, h, :horizontal
         when "vertical" then Layout.regular_3 w, h, :vertical
         when "regular" then Layout.regular_3 w, h, :auto
         when "pruskal" then Layout.pruskal w, h
         when "prim" then Layout.prim w, h
         when "dbt" then Layout.dbt w, h
           exit
         end

ARGV[0].split("-")[0 .. -1].reverse_each do |_lad|
  layout = layout.chordless
end

model = Model.new layout

begin
  puts "\e[?25l"
  IO.console.clear_screen

  puts model.layout.as_maze
  STDIN.gets

  model.layout.capacity.times do |id|
    pos = model.suggest_place
    crate = Crate.rgba2 id
    model.crates[id] = crate
    path = model.layout.path pos
    model.animate_insert crate, [nil, nil] + path.reverse
    model.animate_worker path[2..]
  end
  model.crates.values.reverse_each do |crate|
    path = model.layout.path crate.pos
    model.animate_worker path[2..].reverse
    model.animate_extract crate, path + [nil, nil]
  end
ensure
  puts "\e[?25h"
  IO.console.clear_screen
end
