require 'json'

RAD2DEG = 180 / Math::PI

USE_GEO = ARGV.include?("--use-geo")
USE_LENGTHS = ARGV.include?("--use-length")
CONTINUITY = ARGV.include?("--continuity")
PRIM = ARGV.include?("--prim")
ARGV.replace([])

Dest = Struct.new :ix, :name, :size, :indegree, :outdegree, :lat, :lon do
  def to_s
    if USE_GEO
      "%.6f, %.6f (%s)" % [lat * RAD2DEG, lon * RAD2DEG, name]
    else
      "#{name} (#{outdegree}x out, #{indegree}x in)"
    end
  end
end

class Array
  def weighted_sample(&block)
    weights = map(&block)
    needle = rand * weights.sum
    zip(weights).find{|e, w| needle -= w; needle <= 0}.first
  end
end

class PairEnum
  def initialize(elems)
    @elems = elems
    @exclusions = []
  end
  
  include Enumerable
  def each(&block)
    return each_entry unless block_given?
    @elems.each{|x| @elems.each{|y| yield [x, y] unless x == y || @exclusions.include?([x, y])}}
  end
  
  attr_reader :exclusions
  def delete(pair); @exclusions |= [pair]; end
  def empty?; !any?; end
  
  def weighted_sample
    needle = rand * reduce(0){|a, d| a + yield(d)}
    find{|e| needle -= yield(e); needle <= 0}
  end
  
  def max_by
    reduce([nil, nil]) do |(a, w_a), d|
      w_d = yield d
      w_a.nil? || (w_d <=> w_a) > 0 ? [d, w_d] : [a, w_a]
    end.first
  end
end

def dms_to_radians(str)
  str.split(" ").map(&:to_f).reverse.reduce{|a, d| a/60 + d} * (Math::PI / 180)
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

puts "To gather the desired data, please adjust the following line of Javascript, then run at the appropriate page:"
puts %`copy(JSON.stringify($(".sortable tbody tr").get().map(row => [row.cells[<NAME_COL>], row.cells[<SIZE_COL>]].map(e => e.textContent))))`

input = gets
if(input.strip == "[")
  loop{line = gets; input += line; break if line.strip == "]"}
end

dests = JSON.parse(input, symbolize_names: true).map.with_index do |row, ix|
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
     row[:lat] = dms_to_radians row[:lat]
     row[:lon] = dms_to_radians row[:lon]
   end
   Dest.new(ix, row[:name], row[:size], 0r, 0r, row[:lat], row[:lon])
 end

last_dest = dests[0]

pairs = PairEnum.new dests
if USE_LENGTHS || USE_GEO
  pair_lengths = Array.new(dests.size){|i|Array.new(dests.size){|j| i == j ? 0 : 40000}}
  path_next = Array.new(dests.size){|i|Array.new(dests.size){|j| j}}
end



puts "#{dests.size} destinations loaded, resulting in #{pairs.count} pairs."

until pairs.empty?
  if USE_LENGTHS || USE_GEO
    pair = pairs.max_by do |pair|
      [
        CONTINUITY && pair[0] == last_dest ? 1 : 0,
        PRIM && pair_lengths[0][pair[0].ix] < 40000 ? 1 : 0,
        (USE_LENGTHS || USE_GEO ? pair_lengths[pair[0].ix][pair[1].ix] : 1.0) /  
        (pairs.exclusions.map{|i, j|
          pair_lengths[i.ix][j.ix] - pair_lengths[i.ix][pair[0].ix] - pair_lengths[pair[1].ix][j.ix]
        } + [geo_dist(*pair)]).max, # descending by min length guaranteed by triangle inequality
        USE_GEO ? 0 : rand
      ]
    end
  else
    pair = pairs.weighted_sample do |x, y|
      x.size * (x.indegree + 1) / (x.outdegree + 1) ** 2 *
      y.size * (y.outdegree + 1) / (y.indegree + 1) ** 2
    end
  end
  pair[0].outdegree += 1
  pair[1].indegree += 1
  pairs.delete pair
  last_dest = pair[1]
  geo_dist_str = USE_GEO ? " (distance #{geo_dist(*pair)} km)" : ""
  length_str = if USE_LENGTHS
    path_ixes = [pair[0].ix];
    path_ixes << path_next[path_ixes.last][pair[1].ix] until path_ixes.last == pair[1].ix 
    via_str = path_ixes.length > 2 ? " via #{path_ixes[1..-2].map{|ix| dests[ix].name}.join(", ")}" : ""
    "(shortest path #{pair_lengths[pair[0].ix][pair[1].ix].to_f} km#{via_str})"
  else ""
  end
  puts "from: #{pair[0]} to: #{pair[1]}#{geo_dist_str}#{length_str}"
  
  if USE_LENGTHS || USE_GEO    
    len_pair = USE_LENGTHS ? (print "path length? (km)"; gets.to_r) : geo_dist(*pair)
    
    (0...dests.size).each{|i|(0...dests.size).each{|j|
      len_ij = pair_lengths[i][pair[0].ix] + len_pair + pair_lengths[pair[1].ix][j]
      if pair_lengths[i][j] > len_ij
        puts "path from #{dests[i].name} to #{dests[j].name} shortened from #{pair_lengths[i][j].to_f} to #{len_ij.to_f}"
        pair_lengths[i][j] = len_ij
        path_next[i][j] = i == pair[0].ix ? pair[1].ix : path_next[i][pair[0].ix]
      end
    }}
  end
  
  print "#{pairs.count} pairs remaining"
  break if gets !~ /^$/
end