require 'zlib'
require 'json'
require 'io/console'
require 'stackprof'

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

  def all_tiles; 2 ** tileset.size - 1; end

  # converts an array of tile objects or indices into its packed representation
  def pack_tiles tiles
    tileset.map.with_index{|t, i| tiles.include?(t) || tiles.include?(i) ? 2 ** i : 0}.sum
  end

  # converts the packed representation of a set of tiles into an array of tile objects
  def unpack_tiles tiles
    tileset.select.with_index{|t, i| tiles[i] == 1}
  end

  # unpacks set of tiles, applies a block to each of them and repacks the set
  def map_tiles tiles, &key; pack_tiles unpack_tiles(tiles).map(&key); end

  def to_json
    rule_id_map = Hash[rules.map.with_index{|r, i| [r.id, i]}]
    JSON.generate({
      s: symmetry,
      ts: tile_symm,
      t: tileset.map do |t|
        {
          n: t.name,
          a: t.ascii,
          r: t.rotated,
          m: t.mirrored
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
  ruleset = Ruleset.new(
    json[:s],
    json[:ts] || [],
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

  def sparse
    @sparse ||= tiles.flat_map.with_index do |row, dy|
      row.map.with_index{|tile, dx| [dx, dy, tile]}
    end.select{|_, _, tile| tile != ruleset.all_tiles}
      .map{|x, y, tile| [x, y, ruleset.all_tiles & ~tile]}
      .sort_by{|_, _, tile| -tile.digits(2).count(1)}
  end

  # returns [x, y, c] if the pattern matches at [dx, dy], meaning no c can occur at x, y, or nil if the rule doesn't apply there.
  # # A rule is considered matching if its pattern occurs fully within the board, except for up to one cell.
  # If all cells match, any of them is returned.
  def apply_at(board, x, y)
    r = nil
    sparse.each do |dx, dy, tile|
      if board[y + dy][x + dx] & tile != 0
        if r.nil? && board[y + dy][x + dx] & ~tile != 0
          r = [x + dx, y + dy, board[y + dy][x + dx] & ~tile]
        else
          return nil
        end
      end
    end

    if r.nil?
      [x, y, board[y][x]]
    else
      r
    end
  end

  # returns every poosition where this rule may need to be reevaluated after a change at the given coordinates.
  def instances_by_diff(board, diff_x, diff_y)
    max_x = board[0].length - tiles[0].length
    max_y = board.length - tiles.length
    sparse.map do |dx, dy, tile|
      rx = diff_x - dx; ry = diff_y - dy
      [rx, ry] if rx >= 0 && ry >= 0 && rx <= max_x && ry <= max_y 
    end.compact
  end

  # applies the rule at every poosition that may have been affected by a change at the given coordinates.
  # returns a list of [x, y, c, rx, ry, r] where each [x, y, c] means that no c can occur at [x, y],
  # [rx, ry] are the corresponding coordinates for apply_at and r is the rule.
  def apply_at_diff(board, diff_x, diff_y)
    instances_by_diff(board, diff_x, diff_y).map do |rx, ry|
      diff = apply_at board, rx, ry
      [*diff, rx, ry, self] if diff
    end.compact
  end

  def all_syms
    bitmaps = [tiles]
    case ruleset.symmetry[0]
    when "4"
      3.times do
        bitmaps << bitmaps.last.transpose.map{|row| row.reverse.map{|tile| ruleset.map_tiles(tile, &:rotated)}}
      end
    when "2"
      bitmaps << bitmaps.last.reverse.map{|row| row.reverse.map{|tile| ruleset.map_tiles(tile, &:rotated)}}
    end
    case ruleset.symmetry[1]
    when "-"  then bitmaps += bitmaps.map{|bmp| bmp.reverse.map{|row| row.map{|tile| ruleset.map_tiles(tile, &:mirrored)}}}
    when "\\" then bitmaps += bitmaps.map{|bmp| bmp.transpose.map{|row| row.map{|tile| ruleset.map_tiles(tile, &:mirrored)}}}
    when "|"  then bitmaps += bitmaps.map{|bmp| bmp.transpose.reverse.transpose.map{|row| row.map{|tile| ruleset.map_tiles(tile, &:mirrored)}}}
    when "/"  then bitmaps += bitmaps.map{|bmp| bmp.reverse.transpose.reverse.map{|row| row.map{|tile| ruleset.map_tiles(tile, &:mirrored)}}}
    end

    bitmaps.uniq!
    bitmaps.each do |bitmap|
      ruleset.tile_symm.each do |permutation|
        perm_as_hash = Hash.new{|h, k| h[k] = k}
        permutation.each do |cycle|
          cycle.each_cons(2){|x, y| perm_as_hash[ruleset.tileset[x]] = ruleset.tileset[y]}
          perm_as_hash[ruleset.tileset[cycle.last]] = ruleset.tileset[cycle.first]
        end
        new_bitmap = bitmap.map{|row| row.map{|tile| ruleset.map_tiles(tile, &perm_as_hash)}}
        bitmaps << new_bitmap unless bitmaps.include? new_bitmap
      end
    end

    [self] + bitmaps[1..].map.with_index(1){|bmp, i| Rule.new(ruleset, id + i, [:symm, id], bmp)}
  end

  B64E = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789[]"
  B64D = Hash[B64E.chars.each_with_index.to_a]

  def compressed_tiles
    tiles.map do |row|
      row.map do |tile|
        bits = tile.to_s(2).rjust(ruleset.tileset.length, "0").reverse
          .gsub(/.{1,6}/){|slice| B64E[slice.ljust(6, "0").to_i(2)]}
      end.join
    end.join "/"
  end

  def decompress_tiles
    if tiles.is_a? Array
      self.tiles = tiles.map{|row| row.map{|cell| ruleset.pack_tiles cell}}
    else
      self.tiles = tiles.split("/").map do |row|
        row.chars.each_slice((ruleset.tileset.count - 1) / 6 + 1).map do |tile|
          tile.map{|char| B64D[char].to_s(2).rjust(6, "0")}.join.reverse.to_i(2)
        end
      end
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
        pos = ruleset.unpack_tiles(tiles).map(&:name).join("/")
        neg = "/" + ruleset.unpack_tiles(ruleset.all_tiles & ~tiles).map(&:name).join("/")
        pos.length > neg.length ? neg : pos
      end.chunk{_1}.map do |k, a|
        if a.length == 1 || a.length == 2 && k.length == 1
          a.join " "
        else
          "#{k}*#{a.length}"
        end
      end.join " " 
    end.chunk{_1}.map{|k, a| a.length == 1 ? k : "#{k}**#{a.length}"}.join "\n"

    [h, d].join "\n"
  end

  def summary
    w, h = [tiles.length, tiles[0].length].sort
    "Rule ##{id} (#{sparse.length} of #{w}*#{h}}"
  end
end

# Applies all rules to the given point on the board and any consequent changes
# until no change happens and modifies the stats. If a conflict is detected, returns the new
# rule.
# ruleset - never modified by this function
# board, rule_stats - On happy path, all rules are applied. If a conflict is detected, left unchanged.
# origin_x, origin_y - if set, assumes all rules disjoint with that position have already been applied.
#                      required unless conflict_check_only is set.
# conflict_check_only - if set, rule generation is skipped. Returns true or false indicating whether a conflict was found.
# stop_at - x, y, tiles triple. If set and the given position reached thru given set of possibilities, assumes a conflict.
# renderer - called after each successful rule application on the happy path, and after each successful
#            generalisation while generating a r ule.
def apply_ruleset(ruleset, board, rule_stats, origin_x, origin_y, conflict_check_only = false, stop_at = nil, &renderer)
  return if board.empty? || board[0].empty?
  renderer.call board, rule_stats.values.sum, board.length * board[0].length * (ruleset.tileset.length - 1)
  renderer.call board, rule_stats.values.sum, board.length * board[0].length * (ruleset.tileset.length - 1), [origin_x, origin_x, origin_y, origin_y], hl: true if origin_x
  conflict = false
  undo_log = []
  diff_queue = {}
  diff_queue[[origin_x, origin_y]] = nil if origin_x .is_a? Numeric
  if !origin_x.is_a?(Numeric)
    (origin_x || ruleset.rules).each do |rule|
      (0 .. board.length - rule.tiles.length).each do |rule_y|
        (0 .. board[0].length - rule.tiles[0].length).each do |rule_x|
          diff_x, diff_y, diff_c = rule.apply_at board, rule_x, rule_y
          next unless diff_x
          board[diff_y][diff_x] -= diff_c
          stat_rule = rule.source[0] == :symm ? rule.source[1] : rule.id
          rule_stats[stat_rule] += diff_c.digits(2).count(1)
          conflict = board[diff_y][diff_x] == 0 ||
            stop_at && stop_at[0] == diff_x && stop_at[1] == diff_y && board[diff_y][diff_x] & ~stop_at[2] == 0
          undo_log << [diff_x, diff_y, diff_c, rule_x, rule_y, rule]
            diff_queue[[diff_x, diff_y]] = nil
          renderer.call board, rule_stats.values.sum, board.length * board[0].length * (ruleset.tileset.length - 1), [diff_x, diff_x, diff_y, diff_y]
          break if conflict
        end
        break if conflict
      end
      break if conflict
    end
  end

  while !diff_queue.empty? && !conflict
    prev_diff = diff_queue.keys[0]
    diff_queue.delete prev_diff
    ruleset.rules.each do |rule|
      rule.apply_at_diff(board, *prev_diff).each do |new_diff|
        diff_x, diff_y, diff_c, rule_x, rule_y, _ = new_diff
        board[diff_y][diff_x] &= ~diff_c
        stat_rule = rule.source[0] == :symm ? rule.source[1] : rule.id
        rule_stats[stat_rule] += diff_c.digits(2).count(1)
        conflict = board[diff_y][diff_x] == 0 ||
          stop_at && stop_at[0] == diff_x && stop_at[1] == diff_y && board[diff_y][diff_x] & ~stop_at[2] == 0
        undo_log << new_diff
        diff_queue[[diff_x, diff_y]] = nil
        renderer.call board, rule_stats.values.sum, board.length * board[0].length * (ruleset.tileset.length - 1), [diff_x, diff_x, diff_y, diff_y]
        break if conflict
      end
      break if conflict
    end
  end

  return conflict if conflict_check_only

  return unless conflict

  # rule generation:
  # first, we quickly discard all tiles that didn't even participate in conflict detection;
  # second, we find the last tile participating in conflict detection that by alone
  #   causes a conflict against the board (origin)
  # third, we disccard all tiles that are not required to detect conflict,
  #   starting from least constrained tiles, working in toward the origin point
  # fourth, for each remaining tile, we remove it from the new rule, apply all other rules fully,
  #   and then find the smallest set of constraints that still generates a conflict
  
  # phase one: trace the undo log

  new_rule_tiles = Hash.new{0}
  inferred_tiles = Hash.new(0)
  undo_log.reverse.each.with_index do |entry, entry_ix|
    diff_x, diff_y, diff_c, rule_x, rule_y, rule = entry
    board[diff_y][diff_x] |= diff_c
    next if !new_rule_tiles.empty? && !new_rule_tiles.include?([diff_x, diff_y])
    entry << :x
    stat_rule = rule.source[0] == :symm ? rule.source[1] : rule.id
    inferred_tiles[[diff_x, diff_y]] |= diff_c if entry_ix > 0
    rule.sparse.each do |x, y, c|
      if entry_ix == 0 || diff_x != rule_x + x || diff_y != rule_y + y
        (0..ruleset.tileset.count).each{new_rule_tiles[[rule_x + x, rule_y + y]] |= c}
      end
    end
  end

  new_rule_min_x, new_rule_max_x = (new_rule_tiles.keys.map{|x, _| x} + [origin_x]).minmax
  new_rule_min_y, new_rule_max_y = (new_rule_tiles.keys.map{|_, y| y} + [origin_y]).minmax
  rule_bitmap = [*new_rule_min_y .. new_rule_max_y].map{[*new_rule_min_x .. new_rule_max_x].map{ruleset.all_tiles}}
  new_rule_tiles.keys.each{|x, y| rule_bitmap[y - new_rule_min_y][x - new_rule_min_x] &= board[y][x]}
  renderer.call rule_bitmap, 0, rule_bitmap.length * rule_bitmap[0].length

  # phase two: find the last conflict

  inferred_tiles[[origin_x, origin_y]] = ruleset.all_tiles & ~board[origin_y][origin_x]
  inferred_tiles = inferred_tiles.to_a.sort_by.with_index{|((x, y), c), ix| [(board[y][x]& ~c).digits(2).count, ix]}
  inferred_tiles = inferred_tiles[0 .. inferred_tiles.find_index{|(x, y), _| x == origin_x && y == origin_y}]
  rule_bitmap[origin_y - new_rule_min_y][origin_x - new_rule_min_x] = ruleset.all_tiles
  renderer.call board, 0, new_rule_tiles.length
  (origin_x, origin_y), origin_c = inferred_tiles.find.with_index do |((origin_x, origin_y), origin_c), ix|
    renderer.call rule_bitmap, ix, inferred_tiles.length
    renderer.call rule_bitmap, ix, inferred_tiles.length,
      [origin_x - new_rule_min_x, origin_x - new_rule_min_x,
       origin_y - new_rule_min_y, origin_y - new_rule_min_y], hl: true
    new_bitmap = rule_bitmap.map &:dup
    new_bitmap[origin_y - new_rule_min_y][origin_x - new_rule_min_x] &= ~origin_c
    apply_ruleset(ruleset, new_bitmap, Hash.new(0), origin_x - new_rule_min_x, origin_y - new_rule_min_y, true) {}
  end
  origin_x -= new_rule_min_x
  origin_y -= new_rule_min_y
  rule_bitmap[origin_y][origin_x] &= ~origin_c
  raise "bug" if rule_bitmap[origin_y][origin_x] == 0

  # phase three: discard full tiles

  coord_iter = [*0 ... rule_bitmap.length].product([*0 ... rule_bitmap[0].length])
    .select{|y, x| rule_bitmap[y][x].digits(2).count(1) < ruleset.tileset.count}
    .sort_by{|y, x| [
      rule_bitmap[y][x].digits(2).count(1),
      (origin_x - x) ** 2 + (origin_y - y) ** 2,
      y, x
    ]}.reverse

  renderer.call rule_bitmap, 0, rule_bitmap.length * rule_bitmap[0].length,
    [origin_x, origin_x,origin_y, origin_y], hl: true
  coord_iter.each.with_index do |(y, x), ix|
    y, x = coord_iter[ix]
    renderer.call rule_bitmap, ix + 1, coord_iter.length, [x, x, y, y], hl: true
    new_bitmap = rule_bitmap.map{|row| row.dup}
    new_bitmap[y][x] = ruleset.all_tiles
    if apply_ruleset(ruleset, new_bitmap, Hash.new(0),
                       nil, nil, true, [x, y, rule_bitmap[y][x]]) {}
      rule_bitmap[y][x] = ruleset.all_tiles
    end
    unless x == origin_x && y == origin_y
      renderer.call rule_bitmap, ix + 1, coord_iter.length, [x, x, y, y], hl: false
    end
  end

  # phase four: discard individual constraints

  coord_iter = [*0 ... rule_bitmap.length].product([*0 ... rule_bitmap[0].length])
    .select{|y, x| rule_bitmap[y][x].digits(2).count(1) < ruleset.tileset.count - 1}
    .sort_by{|y, x| [
      rule_bitmap[y][x].digits(2).count(1),
      (origin_x - x) ** 2 + (origin_y - y) ** 2,
      y, x
    ]}.reverse

  coord_iter.each.with_index do |(y, x), ix|
    renderer.call rule_bitmap, ix + 0.0, coord_iter.length, [x, x, y, y], hl: true
    bitmap_without = rule_bitmap.map(&:dup)
    bitmap_without[y][x] = ruleset.all_tiles
    if apply_ruleset(ruleset, bitmap_without, Hash.new(0),
                       nil, nil, true) {}
      raise "There's a conflict if #{[x, y]} is removed. We should have noticed earlier."
    end
    rule_bitmap[y][x] |= ruleset.all_tiles & ~bitmap_without[y][x]
    tile_iter = (0 ... ruleset.tileset.count).select{|tile| rule_bitmap[y][x] & 2 ** tile == 0}
    tile_iter.each.with_index(1) do |tile, tix|
      renderer.call rule_bitmap, ix + tix / (tile_iter.length + 1.0), coord_iter.length, [x, x, y, y], hl: true
      new_bitmap = bitmap_without.map(&:dup)
      new_bitmap[y][x] = rule_bitmap[y][x] | 2 ** tile
      if apply_ruleset(ruleset, new_bitmap, Hash.new(0),
          nil, nil, true, [x, y, rule_bitmap[y][x]]
                      ) {}
        rule_bitmap[y][x] |= 2 ** tile
      end
    end
    renderer.call rule_bitmap, ix + 1.0, coord_iter.length, [x, x, y, y], hl: false
  end

  #we do one last run to collect conflict stats because the previous run may have taken a shortcut
  conflict_stats = Hash.new(0)
  apply_ruleset(ruleset, rule_bitmap.map(&:dup), conflict_stats, nil, nil, true) {}

  rule_bitmap.shift while rule_bitmap.first.all? {|tile| tile == ruleset.all_tiles}
  rule_bitmap.pop while rule_bitmap.last.all? {|tile| tile == ruleset.all_tiles}
  rule_bitmap.each &:shift while rule_bitmap.all?{|row| row.first == ruleset.all_tiles}
  rule_bitmap.each &:pop   while rule_bitmap.all?{|row| row.last  == ruleset.all_tiles}
  Rule.new(ruleset,
           ruleset.rules.map{_1.id}.max + 1,
           [:conflict] + conflict_stats.keys.select{conflict_stats[_1] > 0},
           rule_bitmap)
end

class String
  def display_length; gsub(/\e.*?m/,"").length; end
  def highlight; gsub(/^|\e\[0m/, "\\&\e[7m") + "\e[0m"; end
end

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

  i0 = prompt_tile(name, symm_r, symm_m)
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

  if symm_m.nil?
    return [i0, i1, i2, i3].uniq 
  end

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

  [i0, i1, i2, i3, i0.mirrored, i1.mirrored, i2.mirrored, i3.mirrored].uniq
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

def progress_bar progress, text = "", width 
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

def generate ruleset, method, w, h, seeded, tile = nil
  render = proc do |board, n, d, diff = nil, hl: false|
    print "\e[H\e[?25l"
    full_draw = diff.nil?
    diff ||= [0, board[0].length - 1, 0, board.length - 1]
    tw = ruleset.tileset[0].ascii[0].display_length
    th = ruleset.tileset[0].ascii.length
    (diff[2] .. diff[3]).each do |y|
      (0 ... th).each do |ty|
        print "\e[#{th * y + ty + 1};#{diff[0] * tw + 1}H"
        print (diff[0] .. diff[1]).map{|x|
          n_tiles = board[y][x].digits(2).count(1)
          if n_tiles == 1
            ruleset.unpack_tiles(board[y][x])[0].ascii[ty]
          else
            rgb = if n_tiles == ruleset.tileset.length
                    64
                  else
                    (255 * (2 - n_tiles.fdiv(ruleset.tileset.length)) / 2).to_i
                  end
            str = [board[y][x].digits(2).count(1), 10 ** tw - 1].min.to_s.rjust(tw)
            "\e[38;2;#{rgb};#{rgb};#{rgb}m#{str}\e[0m"
          end
        }.map{hl ? _1.highlight : _1}.join
        print "\e[K" if full_draw
      end
    end
    n_str = n.is_a?(Integer) ? n.to_s : "%.2f" % n
    print "\e[#{board.length * th + 1};1H"
    print progress_bar(n.fdiv(d), "%s / %d" % [n_str, d], IO.console.winsize[1] - 1)
    print "\e[J" if full_draw
    print "\e[?25h"
  end

  tile = ruleset.tileset.find_index tile
  randomization = nil
  loop do
    if randomization.nil? || seeded == :unseeded
      randomization = [*0 ... w].product([*0 ... h]).map do |x, y|
        [x, y, [*0 ... ruleset.tileset.length].map{2 ** _1}.shuffle]
      end
      case method
      when :drizzle
        randomization = randomization.flat_map{|x, y, ts| ts.map{|t| [x, y, ruleset.all_tiles - t]}}.shuffle
      when :rain, :wfcr then randomization.shuffle!
      when :pour, :wfcp then randomization.sort_by!{|x, y, _| (2 * x - w + 1) ** 2 + (2 * y - h + 1) ** 2}
      when :lex, :wfcl then randomization.sort_by!{|x, y, _| [-y, x]}
      else raise "bug: unknown generation method"
      end
      if method != :drizzle
        randomization = randomization.flat_map{|x, y, ts| ts.map{|t| [x, y, t]}}
      end
    end

    board = Array.new(h){Array.new(w){ruleset.all_tiles}}
    stats = Hash[ruleset.rules.select{_1.source[0] != :symm}.map{[_1.id, 0]}]
    stats[:g] = 0
    if apply_ruleset ruleset, board, stats, nil, nil, true, &render
      puts "no solution"
      return
    end

    gen_progress = 0
    loop do
      x, y, t = randomization[gen_progress]
      while board[y][x] & ~t == 0 || board[y][x] & t == 0
        gen_progress += 1
        break if gen_progress == randomization.length
        x, y, t = randomization[gen_progress]
      end
      break if gen_progress == randomization.length
      if method == :wfcr || method == :wfcp || method == :wfcl
        count = board.flat_map{|row| row.map{_1.digits(2).count(1)}}.select{_1 > 1}.min
        x, y, t = randomization[gen_progress..].find do 
          |x, y, t| board[y][x].digits(2).count(1) == count && board[y][x] & ~t != 0 && board[y][x] & t != 0
        end
      end

      new_stats = stats.dup
      new_stats[:g] += (board[y][x] & ~t).digits(2).count(1)
      new_board = board.map(&:dup)
      new_board[y][x] &= t
      new_rule = apply_ruleset ruleset, new_board, new_stats, x, y, &render
      if new_rule
        new_rule_syms = new_rule.all_syms
        ruleset.rules += new_rule_syms
        puts "new rule #{new_rule.summary}; now at #{ruleset.rules.count} rules"
        puts new_rule
        puts "rule stats:"
        puts vwrap stats.to_a
        puts "#{stats.values.sum} total"
        ruleset.rules.sort_by!.with_index do |rule, ix|
          [(rule.source[0] == :symm ? -stats[rule.source[1]] : -stats[rule.id] rescue -stats.values.max - 1), rule.source[0], ix]
        end
        stats[new_rule.id] = 0
        puts "press enter to retry"
        gets
        
        new_board = board.map(&:dup)
        new_rule = apply_ruleset ruleset, new_board, stats, nil, nil, true, &render
        if new_rule
          board[y][x] = 0
          render[board, stats.values.reduce(&:+), board.length * board[0].length * (ruleset.tileset.length)]
          if seeded.nil?
            puts "conflict; aborting"
            return
          else
            puts "conflict; retrying"
            break
          end
        else
          board = new_board
        end
      else
        board = new_board
        stats = new_stats
      end
    end
    puts "rule stats:"
    puts vwrap stats.to_a
    puts "#{stats.values.sum} total"
    ruleset.rules.sort_by!.with_index do |rule, ix|
      [(rule.source[0] == :symm ? -stats[rule.source[1]] : -stats[rule.id] rescue -stats.values.max - 1), rule.source[0], ix]
    end

    rules_deleted = ruleset.rules.select{_1.source[0] == :conflict && stats[_1.id] == 0}.each do |to_delete, _|
      ruleset.rules.select{_1.source[0] == :conflict && _1.source.include?(to_delete.id)}.each do |child_rule|
        child_rule.source = child_rule.source - [to_delete.id] | to_delete.source[1..]
      end
      puts "#{to_delete.summary} purged"
      ruleset.rules.reject!{_1.id == to_delete.id || _1.source == [:symm, to_delete.id]}
    end.length
    puts "#{rules_deleted}/#{stats.length - 1} rules purged"
    ruleset.rules.sort_by!.with_index do |rule, ix|
      [(rule.source[0] == :symm ? -stats[rule.source[1]] : -stats[rule.id] rescue - stats.values.max - 1), rule.source[0], ix]
    end
    if gen_progress == randomization.length
      puts "success"
      return
    end
    gets
  end
end

if $0 == __FILE__
  ruleset = nil
  loop do
    print "WFC> "
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
        tiles.each do |tile|
          tile.rotated = ruleset.tileset.find_index tile.rotated
          tile.mirrored = ruleset.tileset.find_index tile.mirrored
        end
        puts "ok"
      else
        puts "#{error.inspect} already defined, discarding #{tiles.count} tiles"
      end
    when /^add symmetry (.+)$/
      begin
        new_symm = $1.split(" ").map do |cycle_str|
          cycle = cycle_str.split("/").map do |tile_str|
            tile = ruleset.tileset.find_index{_1.name == tile_str}
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
    when /^add rule(?: (.+))?$/
      begin
        raise "no tiles defined" if ruleset.nil? || ruleset.tileset.empty?
        if $1.nil?
          strs = []
          loop{str = gets.chomp; break if str.empty?; strs << str}
        else
          strs = [$1]
        end
        strs = strs
          .flat_map{|str| str.split %r"(?<!/)//(?!/)"}
          .flat_map{|str| str =~ /^(.+)\*\*(\d+)$/ ? [$1] * $2.to_i : [str]}
        raise "rule must include at least one tile" if strs.empty?
        rule_tiles = strs.map do |row_str|
          row_str.gsub!(/(\S+)\*(\d+)/){([$1] * $2.to_i).join " "}
          row_str.split(" ").map do |tile_str|
            names = tile_str.split("/", -1)
            if names == ["", ""]
              ruleset.all_tiles
            elsif names[0] == ""
              tiles = ruleset.tileset.select{names.include? _1.name}
              if tiles.count != names.count - 1
                error = names.select{|name| !ruleset.tileset.any? {_1.name == name}}
                raise "#{error} aren't tiles in this ruleset"
              end
              ruleset.pack_tiles(ruleset.tileset.to_a - tiles)
            else
              tiles = ruleset.tileset.select{names.include? _1.name}
              if tiles.count != names.count
                error = names.select{|name| !ruleset.tileset.any? {_1.name == name}} - [""]
                raise "#{error} aren't tiles in this ruleset"
              end
              ruleset.pack_tiles(tiles)
            end
          end
        end
        raise "pattern must be a rectangle" unless rule_tiles.all?{_1.length == rule_tiles[0].length}
        rule = Rule.new(ruleset, (ruleset.rules.map(&:id).max || -1) + 1, [:axiom], rule_tiles)
        new_rules = rule.all_syms
        ruleset.rules += new_rules
        puts "ok; #{rule.summary} added"
      rescue
        puts $!.message
      end
    when /^delete (cascade )?rule (\d+)$/
      delete_stack = [$2.to_i]
      until delete_stack.empty?
        referrer = ruleset.rules.find do |rule|
          rule.source[0] == :conflict && rule.source[1..].include?(delete_stack.last)
        end
        if referrer
          puts "rule #{delete_stack.last} is referred to by rule #{referrer.id}"
          if $1.nil?
            puts "deleting nothing"
            break
          else
            delete_stack << referrer.id
          end
        else
          puts "deleting rule #{delete_stack.last}"
          ruleset.rules.reject! do |rule|
            rule.id == delete_stack.last || rule.source == [:symm, delete_stack.last]
          end
          delete_stack.pop
        end
      end
      puts "ok; #{ruleset.rules.count} rules remain"
    when /^show (all )?rules$/
      ruleset.rules.each do |rule|
        if $1 || rule.source[0] != :symm
          puts rule
        end
      end
    when /^(gen|genus|gense|generate(?: seeded| unseeded)?) (drizzle|rain|pour|wfc[rpl]|lex)(?: (\d+)x(\d+))?(?: (\S+))?$/
      if ruleset.nil? || ruleset.tileset.empty?
        puts "at least one tile required"
        next
      end
      seeded = case $1
               when "gen", "generate" then nil
               when "gense", "generate seeded" then :seeded
               when "genus", "generate unseeded" then :unseeded
               else raise "error parsing command. This is a bug."
               end
      normalize_tiles ruleset.tileset
      h = $3&.to_i || (IO.console.winsize[0] - 1) / ruleset.tileset[0].ascii.length
      w = $4&.to_i || IO.console.winsize[1] / ruleset.tileset[0].ascii[0].display_length
      tile = ruleset.tileset.find{_1.name == $5}
      if $5 && !tile
        puts "couldn't find tile #{$5}"
        next
      end
      StackProf.start(mode: :cpu)
      begin
        generate ruleset, $2.to_sym, w, h, seeded, tile
      rescue Interrupt
        p $!
      end
      StackProf.stop
    when /^save as (.*)$/
      json = ruleset.to_json
      Zlib::GzipWriter.open($1, level = 9){_1.write json}
      puts "ok; wrote #{File.size $1} bytes (#{json.length} uncompressed)"
    when /^load from (.*)$/
      begin
        Zlib::GzipReader.open($1){ruleset = Ruleset.from_json _1.read}
        puts "ok; #{ruleset.tileset.count} tiles and #{ruleset.rules.count} rules loaded"
      rescue SystemCallError
        puts "cannot load file: #{$!.message.sub(/@ \w+ /, "")}"
      rescue Zlib::GzipFile::Error, JSON::ParserError
        puts "cannot load file: not a valid WFC file"
      end
    when /^quit$/
      StackProf.results("stackprof-output.dump")
      puts "profiling data saved to stackprof-output.dump"
      exit
    when "" then nil
    when /^help$/
      puts <<END
available commands:
new ruleset [124][/-\\|]? - reset all rules and tiles and set the rotation symmetry order and mirror plane for all rules.
add tile (name) - create a tile with a given name. If rules have symmetry, asks for the transformed versions of the tile.
add symmetry (permutation) - define a set of tile substitutions that leave the rules unchanged. Separate tiles within a cycle with /. Separate cycles in a set by spaces.
add rule - define a pattern that may not appear in the generated pattern. Follow by a list of tile names. Separate multiple tiles an the same position with /. Type / followed by a list to include all tiles except the ones listed. Type only / to include all tiles at that position. Use (tiles)*(number) to duplicate that tile in the pattern. Use (row)**(number) to duplicate an entire row. Use // to separate multiple rows of rule pattern in one row of input.
add rule (rule) - as above, but only one line of input.

Ruleset must be defined before tiles, tiles must be defined before tile symmetries that use them, symmetries must be defined before rules.

delete (cascade)? rule (id) - delete a rule. Must not be referenced by other rules. If cascade is set, delete refererrers instead.
show (all)? rules - list all rules in the ruleset. Omits symmetric images of other rules unless specified.

(gen|genus|gense|generate) (seeded|unseeded)? (drizzle|rain|pour|wfc) (wxh)? (tile)? - generates a pattern using the ruleset or finds and adds a rule non-trivially implied by existing rules. Uses the screen size if unspecified. If gense/seeded is set, it attemts to generate the board again with the same RNG if unsuccessful. If genus/unseeded is set, it retries with a different RNG. If neither is set, aborts after one attempt. If tile is specified, it tries to place that tile in the selected position.
  drizzle - at each step, select a random position and remove one possible tile from it
  rain - at each step, select a random position and select one tile for that position
  pour - at each step, sulect an unresolved position closest to the middle and select one tile for that position
  lex - resolve tiles from left to right, then bottom to top
  wfc(r|p|l) - wavefunction collapse classic. At each step, randomly choose a tile with the fewest possibilities and resolve it. The variant decides the order in which tiles with equal number of possibilities are chosen.

save as (filename) - save the ruleset to a file
load from (filename) - restore a ruleset from the file
quit - ends the application without saving
END
    else
      puts "Unknown command. Type help for the list of commands."
    end
  end
end
