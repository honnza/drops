def syms(aspect)
  r = [aspect]
  4.times do
    r << r.last.transpose
    r << r.last.reverse
  end
  r.uniq.sort_by{|aspect| [aspect[0].length, aspect]}
end

class Polyomino
  def width; @aspects[0][0].length; end
  def height; @aspects[0].length; end
  attr_reader :aspects
  
  def hash; @aspects[0].hash; end
  def eql?(other); @aspects[0] == other.aspects[0]; end

  def initialize(bitmap)
    @aspects = syms(bitmap)
  end

  def to_s
    w = width.fdiv(5).ceil
    rows = @aspects[0].map do |row|
      row.reverse.each_slice(5).map{_1.reverse.join.to_i(2).to_s(32)}
    end
    w == 1 ? "Poly##{rows.join}" : "Poly##{rows[0].reverse.join}/#{rows[1..].map(&:reverse).join}"
  end

  def grow
    bitmap = @aspects[0]
    r = []
    (0 ... width).each do |ci|
      if bitmap[0][ci] == 1
        new_bitmap = [bitmap[0].map{0}] + bitmap.map(&:dup)
        new_bitmap[0][ci] = 1
        r << Polyomino.new(new_bitmap)
      end
      if bitmap[-1][ci] == 1
        new_bitmap = bitmap.map(&:dup) + [bitmap[0].map{0}]
        new_bitmap[-1][ci] = 1
        r << Polyomino.new(new_bitmap)
      end
    end
    (0 ... height).each do |ri|
      if bitmap[ri][0] == 1
        new_bitmap = bitmap.map{[0] + _1}
        new_bitmap[ri][0] = 1
        r << Polyomino.new(new_bitmap)
      end
      if bitmap[ri][-1] == 1
        new_bitmap = bitmap.map{_1 + [0]}
        new_bitmap[ri][-1] = 1
        r << Polyomino.new(new_bitmap)
      end
    end
    (0 ... height).each do |ci|
      (0 ... width).each do |ri|
        if bitmap[ri][ci] == 0 && (
            ri > 0 && bitmap[ri-1][ci] == 1 ||
            ri < height - 1 && bitmap[ri+1][ci] == 1 ||
            ci > 0 && bitmap[ri][ci-1] == 1 ||
            ci < width - 1 && bitmap[ri][ci+1] == 1
        )
          new_bitmap = bitmap.map(&:dup)
          new_bitmap[ri][ci] = 1
          r << Polyomino.new(new_bitmap)
        end
      end
    end
    r
  end
end

def gen_polyominoes(n)
  return [] if n == 0
  return [Polyomino.new([[1]])] if n == 1
  gen_polyominoes(n - 1).flat_map(&:grow).uniq
end

if __FILE__ == $0
  case ARGV[0]
  when "gen"
    polys = gen_polyominoes(ARGV[1].to_i).sort_by{|poly| [poly.height, poly.width, poly.to_s]}
    polys.each{puts _1.to_s}
    puts "#{polys.length} polyominoes"
  else puts "unknown method; use \"gen\""
  end
end
