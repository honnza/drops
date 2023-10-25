require 'zlib'
require 'json'
require 'io/console'

Tile = Struct.new(:name, :ascii, :rotated, :mirrored) do
  def inspect;
    "<Tile name = #{name.inspect} ascii = #{ascii.inspect} " +
      "rotated = #{rotated.is_a?(Tile) ? "Tile #{rotated.name}" : rotated.inspect} " +
      "mirrored = #{mirrored.is_a?(Tile) ? "Tile #{mirrored.name}" : mirrored.inspect}"
  end
end

Ruleset = Struct.new(
  :symmetry,  # characterized by the rotation order (1 = no symmetry / 2 / 4), and at most one mirror line (-, /, |, \)
  :tile_symm, # list of tile permutations that generate the ruleset tile symmetries, each stored as a list of cycles
  :tileset,   # list of tiles that can appear in the world
  :rules) do

  def to_json
    tile_map = Hash[tileset.map.with_index{|t, i| [t, i]}]
    rule_id_map = Hash[rules.map.with_index{|r, i| [r.id, i]}]
    JSON.generate({
      s: symmetry,
      ts: tile_symm.map{|p| p.map{|c| c.map{|t| tile_map[t]}}},
      t: tileset.map do |t|
        {
          n: t.name,
          a: t.ascii,
          r: tile_map[t.rotated],
          m: tile_map[t.mirrored]
        }
      end,
      r: rules.map do |r|
        {
          s: [r.source[0]] + r.source[1..].map{|s| rule_id_map[s]},
          t: r.compressed_tiles
        }
      end
    })
  end
end

def Ruleset.from_json str
  json = JSON.parse str, symbolize_names: true
  tiles = json[:t].map{|t| Tile.new(t[:n], t[:a], t[:r], t[:m])}
  tiles.each{|t| t.rotated = tiles[t.rotated.to_i]; t.mirrored = tiles[t.mirrored.to_i]}
  ruleset = Ruleset.new(
    json[:s],
    (json[:ts] || []).map{|p| p.map{|c| c.map{|t| tiles[t.to_i]}}},
    tiles,
    nil
  )
  ruleset.rules = json[:r].map.with_index do |r, i|
    rule = Rule.new(
      ruleset,
      i,
      [r[:s][0].to_sym] + r[:s][1..],
      r[:t]
    )
    rule.decompress_tiles
    rule
  end
  ruleset
end

Rule = Struct.new(
  :ruleset,
  :id,        # unchanging integer identifier within the ruleset
  :source,    # either [:axiom]          - specified by user,
              #        [:symm, id]       - different orientation of another rule,
              #        [#conflict, id..] - impossible configuration proven by some other rules. Must refer to conflict or axiom rules
  :tiles) do # 2d grid of tile sets matching each cell. Any option need to match for the rule to match.

  # returns [x, y, c] if the pattern matches at [dx, dy], meaning no c can occur at x, y, or nil if the rule doesn't apply there.
  # # A rule is considered matching if its pattern occurs fully within the haystack, except for up to one cell.
  # If all cells match, any of them is returned.
  def apply_at(haystack, x, y)
    @sparse ||= tiles.flat_map.with_index do |row, dy|
      row.map.with_index{|tile, dx| [dx, dy, tile]}
    end.select{|_, _, tile| tile != ruleset.tileset}
       .map{|x, y, tile| [x, y, ruleset.tileset - tile]}
       .sort_by{|_, _, tile| - tile.length}

    r = nil
    @sparse.each do |dx, dy, tile|
      unless tile.all?{!haystack[y + dy][x + dx].include?(_1)}
        if r.nil? && haystack[y + dy][x + dx].any?{!tile.include?(_1)}
          r = [x + dx, y + dy, haystack[y + dy][x + dx] - tile]
        else
          return nil
        end
      end
    end

    if r.nil?
      [x, y, haystack[y][x]]
    else
      r
    end
  end

  def all_syms
    bitmaps = [tiles]
    case ruleset.symmetry[0]
    when "4"
      3.times do
        bitmaps << bitmaps.last.transpose.map{|row| row.reverse.map{|tile| tile.map(&:rotated)}}
      end
    when "2"
      bitmaps << bitmaps.last.reverse.map{|row| row.reverse.map{|tile| tile.map(&:rotated)}}
    end
    case ruleset.symmetry[1]
    when "-"  then bitmaps += bitmaps.map{|bmp| bmp.reverse.map{|row| row.map{|tile| tile.map(&:mirrored)}}}
    when "\\" then bitmaps += bitmaps.map{|bmp| bmp.transpose.map{|row| row.map{|tile| tile.map(&:mirrored)}}}
    when "|"  then bitmaps += bitmaps.map{|bmp| bmp.transpose.reverse.transpose.map{|row| row.map{|tile| tile.map(&:mirrored)}}}
    when "/"  then bitmaps += bitmaps.map{|bmp| bmp.reverse.transpose.reverse.map{|row| row.map{|tile| tile.map(&:mirrored)}}}
    end

    bitmaps.each{|bitmap| bitmap.each{|row| row.each{|cell| cell.replace ruleset.tileset & cell}}}
    bitmaps.uniq!
    bitmaps.each do |bitmap|
      ruleset.tile_symm.each do |permutation|
        perm_as_hash = Hash.new{|h, k| h[k] = k}
        permutation.each do |cycle|
          cycle.each_cons(2){|x, y| perm_as_hash[x] = y}
          perm_as_hash[cycle.last] = cycle.first
        end
        new_bitmap = bitmap.map{|row| row.map{|cell| ruleset.tileset & cell.map{|tile| perm_as_hash[tile]}}}
        bitmaps << new_bitmap unless bitmaps.include? new_bitmap
      end
    end

    [self] + bitmaps[1..].map.with_index(1){|bmp, i| Rule.new(ruleset, id + i, [:symm, id], bmp)}
  end

  B64E = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  B64D = Hash[B64E.chars.each_with_index.to_a]

  def compressed_tiles
    tiles.map do |row|
      row.map do |cell|
        ruleset.tileset.map{|tile| cell.include?(tile) ? 1 : 0}
               .each_slice(6).map{|slice| B64E[slice.join.ljust(6, "0").to_i(2)]}.join
      end.join
    end.join "/"
  end

  def decompress_tiles
    if tiles.is_a? String
      self.tiles = tiles.split("/").map do |row|
        row.chars.each_slice((ruleset.tileset.count - 1) / 6 + 1).map do |cell|
          cell.map{|char| B64D[char].to_s(2).rjust(6, "0")}.join
              .chars.map.with_index{|bit, ix| bit == "1" ? ruleset.tileset[ix] : nil}
              .compact
        end
      end
    else
      self.tiles = tiles.map{|tss| tss.map{|ts| ts.map{|t| ruleset.tileset[t.to_i]}}}
    end
  end

  def to_s
    h = "rule #{id} - " + case source[0]
                          when :axiom    then "axiom"
                          when :symm     then "different aspect of #{source[1]}"
                          when :conflict then "conflict of #{source[1..]}"
                          end
    d = tiles.map do |row|
      row.map do |tiles|
        pos = tiles.map(&:name).join("/")
        neg = "/" + (ruleset.tileset - tiles).map(&:name).join("/")
        pos.length > neg.length ? neg : pos
      end.join " "
    end.join "\n"

    [h, d].join "\n"
  end
end

# happy path: Applies all rules to the given point on the board and any consequent changes
# until no change happens and modifies the stats. If a conflict is detected, returns the new
# rule. Otherwise, returns nothing. If conflict check mode is set, returns true if conflict
# has occured, and doesn't generate a new rule
def apply_ruleset(ruleset, board, rule_stats, origin_x, origin_y, conflict_check_only = false, &renderer)
  min_y = origin_y || 0; max_y = origin_y || board.length
  min_x = origin_x || 0; max_x = origin_x || board[0].length
  new_rule_min_y = origin_y || 0; new_rule_max_y = origin_y || board.length
  new_rule_min_x = origin_x || 0; new_rule_max_x = origin_x || board[0].length
  conflict = false
  rules_used = []
  undo_log = []
  loop do
    done = true
    ruleset.rules.each do |rule|
      diff = 0
      min_rule_y = [min_y - rule.tiles.length + 1, 0].max
      max_rule_y = [max_y, board.length - rule.tiles.length].min
      min_rule_x = [min_x - rule.tiles[0].length + 1, 0].max
      max_rule_x = [max_x, board[0].length - rule.tiles[0].length].min
      (min_rule_y .. max_rule_y).each do |rule_y|
        (min_rule_x .. max_rule_x).each do |rule_x|
          diff_x, diff_y, diff_c = rule.apply_at board, rule_x, rule_y
          next unless diff_x
          undo_log << [diff_x, diff_y, diff_c]
          min_x = [min_x, diff_x].min; max_x = [max_x, diff_x].max
          min_y = [min_y, diff_y].min; max_y = [max_y, diff_y].max
          new_rule_min_x = [new_rule_min_x, rule_x].min
          new_rule_min_y = [new_rule_min_y, rule_y].min
          new_rule_max_x = [new_rule_max_x, rule_x + rule.tiles[0].length - 1].max
          new_rule_max_y = [new_rule_max_y, rule_y + rule.tiles.length - 1].max
          board[diff_y][diff_x] -= diff_c
          diff += diff_c.count
          stat_rule = rule.source[0] == :symm ? rule.source[1] : rule.id
          rules_used |= [rule]
          rule_stats[stat_rule] += diff_c.count
          conflict = board[diff_y][diff_x].empty?
          renderer.call board
        end
      end
      break if conflict
      (done = false; break) if diff > 0
    end
    break if done || conflict
  end
  return conflict if conflict_check_only
  return unless conflict

  undo_log.each{|x, y, cs| board[y][x] |= cs}
  rule_bitmap = board[new_rule_min_y .. new_rule_max_y].map{_1[new_rule_min_x .. new_rule_max_x]}
  IO.console.clear_screen
  conflict_stats = rule_stats
  renderer.call rule_bitmap
  coord_iter = [*0 ... rule_bitmap.length].product([*0 ... rule_bitmap[0].length]).sort_by do |y, x|
    (origin_x - new_rule_min_x - x) ** 2 + (origin_y - new_rule_min_y - y) ** 2
  end.reverse
  coord_iter.each do |y, x|
    new_bitmap = rule_bitmap.map{|row| row.map{|tile| tile.dup}}
    next if new_bitmap[y][x] == ruleset.tileset
    new_bitmap[y][x] = ruleset.tileset
    new_stats = Hash.new(0)
    if apply_ruleset(ruleset, new_bitmap, new_stats, origin_x - new_rule_min_x, origin_y - new_rule_min_y, true) {}
      rule_bitmap[y][x] = ruleset.tileset
      renderer.call rule_bitmap
      conflict_stats = new_stats
    end
  end
  coord_iter.each do |y, x|
    ruleset.tileset.each do |tile|
      unless rule_bitmap[y][x].include? tile
        new_bitmap = rule_bitmap.map{|row| row.map{|tile| tile.dup}}
        new_bitmap[y][x] << tile
        new_stats = Hash.new(0)
        if apply_ruleset(ruleset, new_bitmap, new_stats, origin_x - new_rule_min_x, origin_y - new_rule_min_y, true) {}
          rule_bitmap[y][x] << tile
          renderer.call rule_bitmap
          conflict_stats = new_stats
        end
      end
    end
    rule_bitmap[y][x] = ruleset.tileset & rule_bitmap[y][x] # sort bitmap entries by the global tile order
  end

  rule_bitmap.shift while rule_bitmap.first.all? {|tile| tile.length == ruleset.tileset.length}
  rule_bitmap.pop while rule_bitmap.last.all? {|tile| tile.length == ruleset.tileset.length}
  rule_bitmap.each &:shift while rule_bitmap.all?{|row| row.first.length == ruleset.tileset.length}
  rule_bitmap.each &:pop   while rule_bitmap.all?{|row| row.last.length == ruleset.tileset.length}
  Rule.new(ruleset,
           ruleset.rules.map{_1.id}.max + 1,
           [:conflict] + conflict_stats.keys.select{conflict_stats[_1] > 0},
           rule_bitmap)
end

class String; def display_length; gsub(/\e.*?m/,"").length; end; end

def normalize_tiles(tileset)
  h = p tileset.map{|x| x.ascii.length}.max
  w = p tileset.flat_map{|x| x.ascii.map{_1.display_length}}.max
  w = 1 if w == 0
  tileset.each do |tile|
    tile.ascii.each{_1 << ' ' until _1.display_length == w; _1 << "\e[0m" unless _1.end_with? "\e[0m"}
    tile.ascii << ' ' * w until tile.ascii.length == h
  end
end

def prompt_tiles(ruleset, name)
  def prompt_tile(name, rotated, mirrored)
    puts "tile #{name} appearance (use #bbbbbb/ffffff; to set background and foreground color,"
    puts "#bbbbbb; to set only the background, or #; to reset both; use empty line to end input/:"
    ascii = []
    until (line = gets.chomp).empty?
      line.gsub!(/\#(?:(\h{6})(?:\/(\h{6}))?)?\;/) do
        if $1.nil?
          "\e[0m"
        elsif $2.nil?
          "\e[48;2;%d;%d;%dm" % $1.scan(/../).map{_1.to_i(16)}
        else
          "\e[48;2;%d;%d;%dm\e[38;2;%d;%d;%dm" % ($1 + $2).scan(/../).map{_1.to_i(16)}
        end
      end
      ascii << line
    end
    case rotated
    when :"4"
      print "tile rotated clockwise: "
      rotated = gets.chomp
    when :"2"
      print "tile rotated 180 degrees: "
      rotated = gets.chomp
    when :"1"
      rotated = name
    end
    case mirrored
    when :-
      print "tile flipped vertically: "
      mirrored = gets.chomp
    when :|
      print "tile flipped horizontally: "
      mirrored = gets.chomp
    when :/
      print "tile flipped along falling diagonal: "
      mirrored = gets.chomp
    when :"\\"
      print "tile flipped along rising diagonal: "
      mirrored = gets.chomp
    end
    Tile.new name, ascii, rotated, mirrored
  end

  tile_by_name = {}
  tile_by_mirror = {}

  symm_r = ruleset.symmetry[0].to_sym
  symm_m = ruleset.symmetry[1]&.to_sym

  i0 = p prompt_tile(name, symm_r, symm_m)
  tile_by_name[i0.name] = i0
  tile_by_mirror[i0.mirrored] = i0.name
  if i0.rotated.is_a? String
    i0.rotated = tile_by_name[i0.rotated] ||
      prompt_tile(i0.rotated, symm_r == :"2" ? i0 : symm_r, tile_by_mirror[i0.rotated] || symm_m)
  end
  i1 = i0.rotated
  tile_by_name[i1.name] = i1
  tile_by_mirror[i1.mirrored] = i1.name
  if i1.rotated.is_a? String
    i1.rotated = tile_by_name[i1.rotated] ||
      prompt_tile(i1.rotated, symm_r, tile_by_mirror[i1.rotated] || symm_m)
    raise "rotating different tiles shouldn't give the same tile" if i1.rotated == i1
  end
  i2 = i1.rotated
  tile_by_name[i2.name] = i2
  tile_by_mirror[i2.mirrored] = i2.name
  if i2.rotated.is_a? String
    raise "rotating three times shouldn't result in a previous state" if tile_by_name[i2.rotated]
    i2.rotated = prompt_tile(i2.rotated, i0, tile_by_mirror[i2.rotated] || symm_m)
  end
  i3 = i2.rotated
  tile_by_name[i3.name] = i3
  tile_by_mirror[i3.mirrored] = i3.name

  return p [i0, i1, i2, i3].uniq if symm_m.nil?

  if [i0, i1, i2, i3].map{tile_by_name.include? _1.mirrored}.uniq.count != 1
    raise "all mirrored tiles should map to new tiles, or none should"
  end

  i0.mirrored = tile_by_name[i0.mirrored] || prompt_tile(i0.mirrored, nil, i0) if i0.mirrored.is_a? String
  tile_by_name[i0.mirrored.name] = i0.mirrored
  i1.mirrored = tile_by_name[i1.mirrored] || prompt_tile(i1.mirrored, i0.mirrored, i1) if i1.mirrored.is_a? String
  tile_by_name[i1.mirrored.name] = i1.mirrored
  i2.mirrored = tile_by_name[i2.mirrored] || prompt_tile(i2.mirrored, i1.mirrored, i2) if i2.mirrored.is_a? String
  tile_by_name[i2.mirrored.name] = i2.mirrored
  i3.mirrored = tile_by_name[i3.mirrored] || prompt_tile(i3.mirrored, i2.mirrored, i3) if i3.mirrored.is_a? String
  tile_by_name[i3.mirrored.name] = i3.mirrored
  i0.mirrored.rotated = i3.mirrored

  p [i0, i1, i2, i3, i0.mirrored, i1.mirrored, i2.mirrored, i3.mirrored].uniq
end

# formats a table using vertical wrapping. If given an array of arrays, aligns cells with each other vertically.
def vwrap tbl
  return "" if tbl.empty?
  col_widths = (1 .. tbl.length).lazy.map do |n_cols|
    tbl.each_slice((tbl.length - 1) / n_cols + 1).map do |slice|
      n_subcols = slice.map{|row| row.is_a?(Array) ? row.length : 1}.max
      (0 ... n_subcols).map do |ix_subcol|
        slice.map do |row|
          (row.is_a?(Array) ? row[ix_subcol] : ix_subcol == 0 ? row : nil).to_s.length
        end.max
      end
    end
  end.take_while do |col_widths|
    col_widths.flatten.sum + col_widths.flatten.count + 2 * col_widths.count - 1<= IO.console.winsize[1]
  end.to_a.last

  n_rows = (tbl.length - 1) / col_widths.count + 1
  tbl.each_slice(n_rows).to_a.each{_1 << "" until _1.length == n_rows}.transpose.map do |row|
    row.zip(col_widths).map do |subrow, subcol_widths|
      subcol_widths.zip(subrow.is_a?(Array) ? subrow : [subrow]).map{|w, c| c.to_s.ljust w}.join " "
    end.join " | "
  end.join "\n"
end

def generate ruleset, method, w, h, seeded = false
  render = proc do |board|
    print "\e[H\e[?25l"
    tw = board.flatten.compact[0].ascii[0].display_length
    should_puts = tw * w <= IO.console.winsize[1]
    str = ""
    board.each do |br|
      (0 ... br.flatten.compact[0].ascii.length).each do |tri|
        br.each do |bc|
          str << (bc.count == 1 ? bc[0].ascii[tri] : [bc.count, 9].min.to_s * tw)
        end
        str << "\n" if should_puts
      end
    end
    print str
    sleep 0.02
    print "\e[?25h"
  end

  srand
  loop do
    srand Random.seed
    board = Array.new(h){Array.new(w){ruleset.tileset.dup}}
    new_rule = nil
    stats = Hash[ruleset.rules.select{_1.source[0] != :symm}.map{[_1.id, 0]}]
    coord_iter = [*0 ... w].product([*0 ... h]).sort_by{|x, y| (2 * x - w) ** 2 + (2 * y - h) ** 2}.each
    loop do
      coord_iter.next while (x, y = coord_iter.peek; board[y][x].count == 1)
      x, y, sample = loop do
        x, y = method == :pour ? coord_iter.peek : [rand(w), rand(h)]
        sample = if method == :drizzle 
                   ruleset.tileset.sample
                 else
                   ruleset.tileset.dup.shuffle.find{board[y][x].include? _1}
                 end
        break x, y, sample if board[y][x].count > 1 && board[y][x].include?(sample)
      end
      board[y][x] = method == :drizzle ? board[y][x] - [sample] : [sample]

      render[board]
      new_rule = apply_ruleset ruleset, board, stats, x, y, &render
      break if new_rule
    end

    if new_rule
      ruleset.rules += new_rule.all_syms
      puts "new rule found; rule id #{new_rule.id}; now at #{ruleset.rules.count} rules"
      puts new_rule
    else
      puts "success"
    end
    puts "rule stats:"
    puts vwrap stats.to_a
    puts "#{stats.values.sum} total"
    ruleset.rules.sort_by!.with_index do |rule, ix|
      [(rule.source[0] == :symm ? -stats[rule.source[1]] : -stats[rule.id] rescue 0), rule.source[0], ix]
    end
    break if !new_rule || !seeded
    puts "press enter to retry"
    gets
  end
end

if $0 == __FILE__
  ruleset = nil
  loop do
    case gets.chomp
    when /^new ruleset ([124][\\\/\-\|]?)$/
      ruleset = Ruleset.new $1, [], [], []
      puts "ok"
    when /^add tile (\S+)$/
      if !ruleset
        puts "ruleset must be defined before tiles"
        next
      end
      if !ruleset.rules.empty?
        puts "tiles must be defined before rules"
        next
      end
      tiles = prompt_tiles ruleset, $1
      error = tiles.map(&:name) & ruleset.tileset.map(&:name)
      if error.empty?
        ruleset.tileset += tiles
        puts "ok"
      else
        puts "#{error.inspect} already defined, discarding #{tiles.count} tiles"
      end
    when /^add symmetry (.+)$/
      begin
        new_symm = $1.split(" ").map do |cycle_str|
          cycle = cycle_str.split("/").map do |tile_str|
            tile = ruleset.tileset.find{_1.name == tile_str}
            raise "#{tile_str} not found in current ruleset" unless tile
            tile
          end
          if cycle.length == 1
            raise "length-1 cycles don't need to be included in symmetry description. Did you forget a /?"
          end
          cycle
        end
        flat_symm = new_symm.flatten
        if flat_symm.length != flat_symm.uniq.length
          raise "each tile should appear at most once in any tile permutation"
        end
        ruleset.tile_symm << new_symm
        puts "ok"
      rescue
        p $!
        p $@
      end
    when /^add rule$/
      begin
        strs = []
        loop{str = gets.chomp; break if str.empty?; strs << str}
        rule_tiles = strs.map do |row_str|
          row_str.split(" ").map do |tile_str|
            names = tile_str.split("/", -1)
            if names == ["", ""]
              ruleset.tileset
            elsif names[0] == ""
              tiles = ruleset.tileset.select{names.include? _1.name}
              if tiles.count != names.count - 1
                error = names.select{|name| !ruleset.tileset.any? {_1.name == name}}
                puts "#{error} aren't tiles in this ruleset"
                raise
              end
              ruleset.tileset - tiles
            else
              tiles = ruleset.tileset.select{names.include? _1.name}
              if tiles.count != names.count
                error = names.select{|name| !ruleset.tileset.any? {_1.name == name}}
                puts "#{error} aren't tiles in this ruleset"
                raise
              end
              tiles
            end
          end
        end
        rule = Rule.new(ruleset, (ruleset.rules.map(&:id).max || -1) + 1, [:axiom], rule_tiles)
        new_rules = rule.all_syms
        ruleset.rules += new_rules
        puts "ok; #{new_rules.count} new rules added"
      rescue
        p $!
        p $@
      end
    when /^show rules$/
      ruleset.rules.each do |rule|
        unless rule.source[0] == :symm
          puts rule
        end
      end
    when /^generate (seeded )?(drizzle|rain|pour)( (\d+)x(\d+))?$/
      if ruleset.tileset.empty?
        puts "at least one tile required"
        next
      end
      normalize_tiles ruleset.tileset
      h = $3&.to_i || (IO.console.winsize[0] - 1) / ruleset.tileset[0].ascii.length
      w = $4&.to_i || IO.console.winsize[1] / ruleset.tileset[0].ascii[0].display_length
      begin
        generate ruleset, $2.to_sym, w, h, !$1.nil?
      rescue Interrupt
        puts "\e#{h};1Huser abort"
      end
    when /^save as (.*)$/
      json = ruleset.to_json
      Zlib::GzipWriter.open($1, level = 9){_1.write json}
      puts "ok; wrote #{File.size $1} bytes (#{json.length} uncompressed)"
    when /^load from (.*)$/
      Zlib::GzipReader.open($1){ruleset = Ruleset.from_json _1.read}
      puts "ok; #{ruleset.tileset.count} tiles and #{ruleset.rules.count} rules loaded"
    when /^quit$/
      exit
    else
      puts <<END
available commands:
new ruleset [124][/-\\|]? - reset all rules and tiles and set the rotation symmetry order and mirror plane for all rules.
add tile (name) - create a tile with a given name. If rules have symmetry, asks for the transformed versions of the tile.
add symmetry (permutation) - define a set of tile substitutions that leave the rules unchanged. Separate tiles within a cycle with /. Separate cycles in a set by spaces.
add rule - define a pattern that may not appear in the generated pattern. Follow by a list of tile names. Separate multiple tiles an the same position with /. Type / followed by a list to include all tiles except the ones listed. Type only / to include all tiles at that position.

Ruleset must be defined before tiles, tiles must be defined before tile symmetries that use them, symmetries must be defined before rules.

show rules - list all rules in the ruleset

generate (seeded)? (drizzle|rain|pour) (wxh)? - generates a pattern using the ruleset or finds and adds a rule non-trivially implied by existing rules. Uses the screen size if unspecified. If seeded is set, it attemts to generate the board again with the same RNG if unsuccessful.
  drizzle - at each step, select a random position and remove one possible tile from it
  rain - at each step, select a random position and select one tile for that position
  pour - at each step, sulect an unresolved position closest to the middle and select one tile for that position

save as (filename) - save the ruleset to a file
load from (filename) - restore a ruleset from the file
quit - ends the application without saving
END
    end
  end
end
