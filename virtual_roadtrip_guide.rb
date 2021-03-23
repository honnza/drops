require 'io/console'
require 'json'

RAD2DEG = 180 / Math::PI

USE_GEO = ARGV.include?("--use-geo") || ARGV.include?("-g")
USE_LENGTHS = ARGV.include?("--use-length") || ARGV.include?("-l")
CONTINUITY = ARGV.include?("--continuity") || ARGV.include?("-c")
PRIM = ARGV.include?("--prim") || ARGV.include?("-p")
MULTIMST = ARGV.include?("--multi-mst")
ARGV.replace([])

Dest = Struct.new :ix, :name, :size, :indegree, :outdegree, :lat, :lon do
  def to_s
    if lat && lon
      "%.6f, %.6f (%s)" % [lat * RAD2DEG, lon * RAD2DEG, name]
    else
      "#{name} (#{outdegree}x out, #{indegree}x in)"
    end
  end
  
  def to_hash; {name: name, lat: lat, lon: lon}; end
end

class Array
  def weighted_sample(from = 0, &block)
    haystack = from ? filter{|f, t| f == from} : self
    weights = haystack.map(&block)
    needle = rand * weights.sum
    haystack.zip(weights).find{|e, w| needle -= w; needle <= 0}.first
  end
end

class PairEnum
  def initialize(elems)
    @elems = elems
    @exclusions = []
  end

  include Enumerable
  def each(&block)
    p caller
    return each_entry unless block_given?
    @elems.each{|x| @elems.each{|y| yield [x, y] unless x == y || @exclusions.include?([x, y])}}
  end

  attr_reader :exclusions
  def delete(pair); @exclusions |= [pair]; end
  def empty?; !any?; end

  def weighted_sample(from = nil)
    haystack = from ? filter{|f, t| f == from} : self
    needle = rand * haystack.reduce(0){|a, d| a + yield(d)}
    haystack.find{|e| needle -= yield(e); needle <= 0}
  end

  def max_by
    reduce([nil, nil]) do |(a, w_a), d|
      w_d = yield d
      w_a.nil? || (w_d <=> w_a) > 0 ? [d, w_d] : [a, w_a]
    end.first
  end
end

def dms_to_radians(str)
  raise "invalid dms string #{str}" unless /([+-]?)([\d. ]+)/ =~ str
  ($1 == '-' ? -1 : 1) * $2.split(" ").map(&:to_f).reverse.reduce{|a, d| a/60 + d} * (Math::PI / 180)
end

EARTH_DIAMETER = 2 * 6371 # uses mean Earth radius
def geo_dist(from, to)
  return 40000 if [from.lat, from.lon, to.lat, to.lon].any?(&:nil?)
  EARTH_DIAMETER * Math.asin(Math.sqrt(
    Math.cos(from.lat) * Math.cos(to.lat) *
    Math.sin((from.lon - to.lon)/2) ** 2 +
    Math.sin((from.lat - to.lat)/2) ** 2
  ))
end

def azimuth_str(from, to)
  sdl = Math.sin(to.lon - from.lon)
  cdl = Math.cos(to.lon - from.lon)
  tan_a_from =  sdl / (Math.cos(from.lat) * Math.tan(  to.lat) - Math.sin(from.lat) * cdl)
  tan_a_to   = -sdl / (Math.cos(  to.lat) * Math.tan(from.lat) - Math.sin(  to.lat) * cdl)
  from_str = case
  when tan_a_from.abs() > 2.0 then sdl > 0 ? "E" : "W"
  when tan_a_from.abs() < 0.5 then to.lat > from.lat ? "N" : "S"
  when tan_a_from > 0 then sdl > 0 ? "NE" : "SW"
  else sdl > 0 ? "SE" : "NW"
  end
  to_str = case
  when tan_a_to.abs() > 2.0 then sdl > 0 ? "E" : "W"
  when tan_a_to.abs() < 0.5 then to.lat > from.lat ? "N" : "S"
  when tan_a_to > 0 then sdl > 0 ? "NE" : "SW"
  else sdl > 0 ? "SE" : "NW"
  end
  puts "#{tan_a_from} #{tan_a_to}"
  from_str == to_str ? from_str : "#{from_str}..#{to_str}"
end

###############################################################################

input = gets("\n\n")
dests = JSON.parse(input, symbolize_names: true) rescue input.chomp.lines.map(&:chomp)
dests.map!.with_index do |row, ix|
  row = [row, 1] if row.is_a?(String)
  row = {name: row[0], size: row[1]} if row.is_a?(Array)
  size = size.tr("^0-9.", "").to_f if size.is_a?(String)

  if row[:lat].nil? && USE_GEO
    print "#{row[:name]} north latitude? (D M S.S or D M.M or D.D) "
    row[:lat] = gets
  end
  if row[:lon].nil? && USE_GEO
    print "#{row[:name]} east longitude? (D M S.S or D M.M or D.D) "
    row[:lon] = gets
  end

  if USE_GEO
    row[:lat] = dms_to_radians row[:lat] if row[:lat].is_a? String
    row[:lon] = dms_to_radians row[:lon] if row[:lon].is_a? String
  end
  Dest.new(ix, row[:name], row[:size], 0r, 0r, row[:lat], row[:lon])
end

puts JSON.generate(dests.map(&:to_hash))

last_dest = dests[0]

pairs = dests.product(dests).reject{|x, y| x == y}
used_pairs = []
if USE_LENGTHS || USE_GEO
  pair_lengths = Array.new(dests.size){|i| Array.new(dests.size){|j| i == j ? 0 : 40000}}
  path_next = Array.new(dests.size){|i| Array.new(dests.size){|j| j}}
end



puts "#{dests.size} destinations loaded, resulting in #{pairs.count} pairs."

plan = []
if USE_GEO && PRIM # todo: figrue out what the user wants when prim and not use_geo
  pairs.sort_by!{|x, y| geo_dist(x, y)}
  tree_parents = []
  subtree_costs = dests.map{0}
  (dests.count - 1).times do
    x, y = pairs.find{|x, y| (x.ix == 0 || tree_parents[x.ix]) && y.ix != 0 && !tree_parents[y.ix]}
    tree_parents[y.ix] = x
    pair_len = geo_dist(x, y)
    while y.ix != 0 
      subtree_costs[y.ix] += pair_len
      y = tree_parents[y.ix]
    end
  end
  plan_subtree = lambda do |indent, ix|
    subtrees = (0 ... dests.count).filter{|cix| cix != 0 && tree_parents[cix].ix == ix}
      .sort_by{|cix| subtree_costs[cix]}
    
    subtrees.each{|cix|
        prefix = "┃" * indent + (cix == subtrees.last ? "" : "┣");
        plan << [prefix, [dests[ix], dests[cix]]]
        plan_subtree[cix == subtrees.last ? indent : indent + 1, cix]
        plan << [prefix, [dests[cix], dests[ix]]]
      }
  end
  plan_subtree[0, 0]
end

until pairs.empty?
  if !plan.empty?
    prefix, pair = plan.shift
  elsif USE_LENGTHS || USE_GEO
    prefix = ""
    pair = pairs.max_by do |pair|
      [
        CONTINUITY && pair[0] == last_dest ? 1 : 0,
        PRIM && pair_lengths[0][pair[0].ix] < 40000 ? 1 : 0,
        (USE_LENGTHS || USE_GEO ? pair_lengths[pair[0].ix][pair[1].ix] : 1.0) /  
        (USE_LENGTHS ? (used_pairs.map{|i, j|
          pair_lengths[i.ix][j.ix] - pair_lengths[i.ix][pair[0].ix] - pair_lengths[pair[1].ix][j.ix]
        } + [geo_dist(*pair)]).max : geo_dist(*pair)), # descending by min length guaranteed by triangle inequality
        USE_GEO ? 0 : rand
      ]
    end
  else
    prefix = ""
    pair = pairs.weighted_sample(CONTINUITY ? last_dest : nil) do |x, y|
      (x.size || 1) * (x.indegree + 1) / (x.outdegree + 1) ** 2 *
      (y.size || 1) * (y.outdegree + 1) / (y.indegree + 1) ** 2
    end
  end
  if MULTIMST && pair_lengths[pair[0].ix][pair[1].ix] < 40000
    pair_lengths = Array.new(dests.size){|i|Array.new(dests.size){|j| i == j ? 0 : 40000}}
    puts "MST done"
    redo
  end
  pair[0].outdegree += 1
  pair[1].indegree += 1
  pairs.delete pair
  used_pairs << pair
  last_dest = pair[1]
  geo_dist_str = USE_GEO ? " (distance #{geo_dist(*pair)} km)" : ""
  length_str = if USE_LENGTHS
    path_ixes = [pair[0].ix];
    path_ixes << path_next[path_ixes.last][pair[1].ix] until path_ixes.last == pair[1].ix 
    via_str = path_ixes.length > 2 ? " via #{path_ixes[1..-2].map{|ix| dests[ix].name}.join(", ")}" : ""
    "(shortest path #{pair_lengths[pair[0].ix][pair[1].ix].to_f} km#{via_str})"
  else ""
  end

  puts "from: #{pair[0]} to: #{pair[1]}#{geo_dist_str}#{length_str}" if USE_LENGTHS
  if USE_LENGTHS || USE_GEO    
    len_pair = USE_LENGTHS ? (print "path length? (km) "; gets.to_r) : geo_dist(*pair)

    shortenings = []
    (0...dests.size).each{|i|(0...dests.size).each{|j|
      len_ij = pair_lengths[i][pair[0].ix] + len_pair + pair_lengths[pair[1].ix][j]
      if pair_lengths[i][j] > len_ij
        shortenings << [dests[i].name, dests[j].name, pair_lengths[i][j], len_ij]
        pair_lengths[i][j] = len_ij
        path_next[i][j] = i == pair[0].ix ? pair[1].ix : path_next[i][pair[0].ix]
      end
    }}
    if shortenings.count < IO.console.winsize[0]
      shortenings.each do |iname, jname, old_len, new_len| 
        puts "path from #{iname} to #{jname} shortened from #{old_len} to #{new_len}"
      end
    else
      inames = shortenings.map{_1[0]}.uniq
      jnames = shortenings.map{_1[1]}.uniq
      puts "#{shortenings.count} paths from \e[1m#{inames.count}\e[0m places (#{inames.join(", ")}) " +
            "to \e[1m#{jnames.count}\e[0m places (#{jnames.join(", ")}) shortened"
    end
  end

  if !USE_LENGTHS
    puts "#{prefix}from: #{pair[0]} to: #{pair[1]}#{geo_dist_str}#{length_str} #{azimuth_str(*pair)}"
  end

  print "#{pairs.count} pairs remaining"
  break if gets !~ /^$/
end