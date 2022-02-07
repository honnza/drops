require 'io/console'
require 'json'
require 'uri'

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
  raise "invalid dms string #{str}" unless /([+-]?)([\d. ]+)([NEWS]?)/ =~ str
  ($1 == '-' ? -1 : 1) * ($3 == "S" || $3 == "W" ? -1 : 1) *
  $2.split(" ").map(&:to_f).reverse.reduce{|a, d| a/60 + d} * (Math::PI / 180)
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
  return "" unless USE_GEO
  sdl = Math.sin(to.lon - from.lon)
  cdl = Math.cos(to.lon - from.lon)
  tan_a_from =  sdl / (Math.cos(from.lat) * Math.tan(  to.lat) - Math.sin(from.lat) * cdl)
  tan_a_to   = -sdl / (Math.cos(  to.lat) * Math.tan(from.lat) - Math.sin(  to.lat) * cdl)
  eastish = sdl > 0
  if tan_a_from == 0 || tan_a_to == 0
    raise "tan = 0 but to.lon != from.lon" unless to.lon == from.lon
    raise "#{to.name} and #{from.name} have the exact same coordinates" if to.lat == from.lat
    return to.lat > from.lat ? "N" : "S"
  end
  northish_from = eastish ^ (tan_a_from < 0)
  northish_to = eastish ^ (tan_a_to < 0)
  from_str = case
  when tan_a_from.abs() > 2.0 then eastish ? "E" : "W"
  when tan_a_from.abs() < 0.5 then northish_from ? "N" : "S"
  when tan_a_from > 0 then eastish ? "NE" : "SW"
  else eastish ? "SE" : "NW"
  end
  to_str = case
  when tan_a_to.abs() > 2.0 then eastish ? "E" : "W"
  when tan_a_to.abs() < 0.5 then northish_to ? "N" : "S"
  when tan_a_to > 0 then eastish ? "NE" : "SW"
  else eastish ? "SE" : "NW"
  end
  puts "#{tan_a_from} #{tan_a_to}"
  from_str == to_str ? from_str : "#{from_str}..#{to_str}"
end

###############################################################################

input = gets("\n\n")
dests = JSON.parse(input.tr("\n", ""), symbolize_names: true) rescue input.chomp.lines.map(&:chomp)

at_exit do
  puts JSON.generate(dests.map{_1.is_a?(Dest) ? _1.to_hash : _1})
            .gsub(/.{800}/, "\\&\n")
end

def try_parse_geohack_url(uri)
  uri = URI(uri) rescue (return nil)
  return nil unless uri.scheme
  # apparently a single word is a valid URI according to the constructor ¯\_(ツ)_/¯
  puts "unexpected URL host #{uri.host}" unless uri.host == "geohack.toolforge.org"
  puts "unexpected URL path #{uri.path}" unless uri.path == "/geohack.php"
  raise "no query string in #{uri}" unless uri.query
  query = Hash[URI.decode_www_form uri.query.tr("_", " ")]
  raise "no name param in #{uri}" unless query["pagename"]
  raise "no params param in #{uri}" unless query["params"]
  name = query["pagename"]
  latlon_str = query["params"][/^([^:]+ )(?:$|[^: ]+:)/, 1]
  unless latlon_str =~ /([\d. ]+[NS]) ([\d. ]+[EW]) /
    raise "unknown lat/lon string #{latlon_str} in #{uri}"
  end
  {name: name, lat: dms_to_radians($1), lon: dms_to_radians($2)}
end

dests.map!.with_index do |row, ix|
  row = try_parse_geohack_url(row) || [row, 1] if row.is_a?(String)
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

puts JSON.generate(dests.map(&:to_hash)).gsub(/.{800}/, "\\&\n")


last_dest = dests[0]

pairs = dests.product(dests).reject{|x, y| x == y}
used_pairs = []
if USE_LENGTHS || USE_GEO
  pair_lengths = Array.new(dests.size){|i| Array.new(dests.size){|j| i == j ? 0 : 40000}}
  path_next = Array.new(dests.size){|i| Array.new(dests.size){|j| j}}
end



puts "#{dests.size} destinations loaded, resulting in #{pairs.count} pairs."

def squeeze_path_str(str, len)
  return str if str.length <= len
  
  while str.sub!(/(( - \([^)]*\))\2+)/){" - #{$1.length / $2.length}x #{$2[3..]}"}
    return str if str.length <= len
  end

  while (str.sub!(/(?:(\d+)x )?\((\w+)\/\2\) - (?:(\d+)x )?\((\w+)\/\4\)/) do 
    "#{($1&.to_i||1) + ($3&.to_i||1)}x (n/n)"
  end)
    return str if str.length <= len
  end

  raise "TODO: how to squeeze #{str.inspect}"
end

plan = nil
until pairs.empty?
  if USE_GEO && PRIM && (plan.nil? || MULTIMST && plan.empty?)
    # todo: figure out what the user wants when prim and not use_geo
    pair_lengths = Array.new(dests.size){|i|Array.new(dests.size){|j| i == j ? 0 : 40000}}
    plan = []
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
    plan_subtree = lambda do |path_str, pid|
      subtrees = (0 ... dests.count).filter{|cid| cid != 0 && tree_parents[cid].ix == pid}
        .sort_by{|cid| subtree_costs[cid]}
      
      subtrees.each.with_index(1) do |cid, ix|
        grandchildren = (0 ... dests.count).count{|gid| gid != 0 && tree_parents[gid].ix == cid}
        subpath_str = "#{path_str} - (#{ix}/#{subtrees.count})"
        ww = IO.console.winsize[1]
        plan << [squeeze_path_str("#{subpath_str} > (#{grandchildren})\n", ww), [dests[pid], dests[cid]]]
        plan_subtree[subpath_str, cid]
        plan << [squeeze_path_str("#{subpath_str} < (#{grandchildren})\n", ww), [dests[cid], dests[pid]]]
      end
    end
    plan_subtree["", 0]
  end

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
  geo_dist_str = USE_GEO ? "\ndistance #{geo_dist(*pair)} km" : ""
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
    if shortenings.count < IO.console.winsize[0] - 6
      shortenings.each do |iname, jname, old_len, new_len| 
        puts "path from #{iname} to #{jname} shortened from #{old_len} to #{new_len}"
      end
    end
      inames = shortenings.map{_1[0]}.uniq
      jnames = shortenings.map{_1[1]}.uniq
      puts "\e[36m#{shortenings.count}\e[0m paths from \e[36m#{inames.count}\e[0m places (#{inames.join(", ")}) " +
            "to \e[36m#{jnames.count}\e[0m places (#{jnames.join(", ")}) shortened"
  end

  if !USE_LENGTHS
    puts "#{prefix}from: #{pair[0]} to: #{pair[1]}#{geo_dist_str}#{length_str} #{azimuth_str(*pair)}"
  end

  print "#{pairs.count} pairs remaining"
  break if gets !~ /^$/
end