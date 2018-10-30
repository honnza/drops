class Sandpile
 def initialize(ary)
  ary = ary.split("/").map{|r|r.chars.map{|c|c.to_i}} if ary.is_a? String
  @ary = ary
  normalize
 end

 attr_reader :ary, :normalize_trace

 def +(other)
  Sandpile.new(ary.zip(other.ary).map{|r1, r2|
   r1.zip(r2).map{|e1, e2| e1 + e2}
  })
 end

 def *(other)
  ([self]*other).reduce(&:+)
 end

 def inspect
  alphabet = [*?0 .. ?9, *?A .. ?Z]
  @ary.map{|r| r.map{|e| alphabet[e]}.join}.join("/")
 end

 def normalize
  @normalize_trace = @ary.map{|r| r.map{0}}

  loop do
   i = @ary.find_index{|r| r.any?{|e| e > 3}}
   return if i.nil?
   j = @ary[i].find_index{|e| e > 3}
   
   @ary[i][j] -= 4
   @ary[i][j-1] += 1 if j > 0 && ary[i][j-1]
   @ary[i-1][j] += 1 if i > 0 && ary[i-1][j]
   @ary[i][j+1] += 1 if @ary[i][j+1]
   @ary[i+1][j] += 1 if @ary[i+1] && ary[i+1][i]
   @normalize_trace[i][j] += 1
  end
 end

 def self.rect(x, y, c, e, f)
  new (c + e*(x-2) + c + "/") + 
      (e + f*(x-2) + e + "/")*(y-2) +
      (c + e * (x-2) + c)
 end
end
