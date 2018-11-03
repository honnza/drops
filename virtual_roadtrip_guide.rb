require 'json'

USE_GEO = ARGV.include?("--use-geo")
USE_LENGTHS = ARGV.include?("--use-length")
ARGV.replace([])

Dest = Struct.new :ix, :name, :size, :indegree, :outdegree, :lat, :lon do
  def to_s; "#{name} (#{outdegree}x out, #{indegree}x in)"; end
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
  
  def delete(pair); @exclusions |= [pair]; end
  def empty?; !any?; end
  
  def weighted_sample(&block)
    needle = rand * reduce(0){|a, d| a + yield(d)}
    find{|e| needle -= yield(e); needle <= 0}
  end
end

def dms_to_radians(str)
  str.split(" ").map(&:to_f).reverse.reduce{|a, d| a/60 + d} * (Math::PI / 180)
end

EARTH_DIAMETER = 2 * 6371 # uses mean Earth radius
def geo_dist(from, to)
  return 40000 if [from.lat, from.lon, to.lat, to.lon].any?(&:nil?)
  EARTH_DIAMETER * Math.asin(Math.sqrt(
    Math.cos(from.lon) * Math.cos(to.lon) *
    Math.sin((from.lat - to.lat)/2) ** 2 +
    Math.sin((from.lon - to.lon)/2) ** 2
  ))
end

puts "To gather the desired data, please adjust the following line of Javascript, then run at the appropriate page:"
puts %`copy(JSON.stringify($(".sortable tbody tr").get().map(row => [row.cells[<NAME_COL>], row.cells[<SIZE_COL>]].map(e => e.textContent))))`

dests = JSON.parse(gets).map.with_index do |row, ix|
   row = [row, 1] if row.is_a?(String)
   row = {name: row[0], size: row[1]} if row.is_a?(Array)
   size = size.tr("^0-9.", "").to_f if size.is_a?(String)
   
   if row[:lat].nil? && USE_GEO
     print "#{row[:name]} east latitude? (D M S.S or D M.M or D.D) "
     row[:lat] = gets
   end
   if row[:lon].nil? && USE_GEO
     print "#{row[:name]} north longitude? (D M S.S or D M.M or D.D) "
     row[:lon] = gets
   end
   
   row[:lat] = dms_to_radians row[:lat]
   row[:lon] = dms_to_radians row[:lon]
   Dest.new(ix, row[:name], row[:size], 0r, 0r, row[:lat], row[:lon])
 end
pairs = PairEnum.new dests
if USE_LENGTHS
  pair_lengths = Array.new(dests.size){|i|Array.new(dests.size){|j| i == j ? 0 : 40000}}
end

puts "#{dests.size} destinations loaded, resulting in #{pairs.count} pairs."

until pairs.empty?
  if USE_LENGTHS
    pair = pairs.reduce do |a, d| 
      pair_lengths[a[0].ix][a[1].ix] / geo_dist(*a) < pair_lengths[d[0].ix][d[1].ix] / geo_dist(*d) ? d : a
    end
  elsif USE_GEO
    pair = pairs.reduce{|a, d| geo_dist(*a) > geo_dist(*d) ? d : a}
  else
    pair = pairs.weighted_sample do |x, y|
      x.size * (x.indegree + 1) / (x.outdegree + 1) ** 2 *
      y.size * (y.outdegree + 1) / (y.indegree + 1) ** 2
    end
  end
  pair[0].outdegree += 1
  pair[1].indegree += 1
  pairs.delete pair
  geo_dist_str = USE_GEO ? " (distance #{geo_dist(*pair)} km)" : ""
  length_str = USE_LENGTHS ? "(shortest path #{pair_lengths[pair[0].ix][pair[1].ix]} min)" : ""
  puts "#{pair[0]} => #{pair[1]}#{geo_dist_str}#{length_str}"
  
  if USE_LENGTHS
    print "path length? (min) "
    len = gets.to_f
    (0...dests.size).each{|i|(0...dests.size).each{|j|
      pair_lengths[i][j] = [
        pair_lengths[i][j],
        pair_lengths[i][pair[0].ix] + len + pair_lengths[pair[1].ix][j]
      ].min
    }}
  end
  
  print "#{pairs.count} pairs remaining"
  break if gets !~ /^$/
end