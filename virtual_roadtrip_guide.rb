require 'json'

Dest = Struct.new :name, :size, :indegree, :outdegree do
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

puts "To gather the desired data, please adjust the following line of Javascript, then run at the appropriate page:"
puts %`copy(JSON.stringify($(".sortable tbody tr").get().map(row => [row.cells[<NAME_COL>], row.cells[<SIZE_COL>]].map(e => e.textContent))))`

dests = JSON.parse(gets).map do |row|
   name, size = row.is_a?(String) ? [row, 1] : row
   size = size.tr("^0-9.", "").to_f if size.is_a?(String)
   Dest.new(name, size, 0r, 0r)
 end
pairs = PairEnum.new dests

puts "#{dests.size} destinations loaded, resulting in #{pairs.count} pairs."

until pairs.empty?
  pair = pairs.weighted_sample{|x, y| x.size * (x.indegree + 1) / (x.outdegree + 1) ** 2 * y.size * (y.outdegree + 1) / (y.indegree + 1) ** 2}
  pair[0].outdegree += 1
  pair[1].indegree += 1
  pairs.delete pair
  puts pair.join (" => ")
  print "#{pairs.count} pairs remaining"
  break if gets !~ /^$/
end