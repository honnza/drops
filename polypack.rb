require "io/console"

def syms(aspect)
  r = [aspect]
  4.times do
    r << r.last.transpose
    r << r.last.reverse
  end
  r = r.uniq.sort_by{|aspect| [aspect[0].length, aspect]}
  r.select{|aspect| aspect[0].length == r[0][0].length}
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

  # outputs the polyomino as its bitmask code
  def bitmask(aspect = 0)
    w = width.fdiv(5).ceil
    rows = @aspects[aspect].map do |row|
      row.reverse.each_slice(5).map{_1.reverse.join.to_i(2).to_s(32).upcase}
    end
    w == 1 ? rows.join : "#{rows[0].reverse.join}/#{rows[1..].map(&:reverse).join}"
  end

  def to_s; "Poly#{bitmask}"; end

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

class Placement
  def initialize(polyomino, aspect, offset)
    @polyomino = polyomino
    @aspect = aspect
    @offset = offset
  end

  attr_accessor :polyomino, :aspect, :offset
  # polyomino aspect that is used for this placement
  def oriented; polyomino.aspects[aspect]; end
  # index of first unoccupied column after the polyomino
  def right; polyomino.width + offset; end
  def bitmask; polyomino.bitmask(aspect); end
  def to_s; "#{polyomino.to_s(aspect)}\e[30;1m@\e[0m#{offset}"; end

  # space taken up by the polyomino and to the left of it
  def c_cost; polyomino.height * offset + polyomino.aspects[aspect].map{|row| row.rindex(0) + 1}; end
end

Strip = Struct.new :placements do
  def initialize(placements = []); @placements = placements; end
  attr_accessor :placements
  def height; placements.last.polyomino.height; end
  def width; placements.last.right; end
  def to_s; placements.join(", "); end
  def bitmasks; placements.map(&:bitmask).join " "; end

  # leftmost placement of a given aspect that doesn't overlap the previous orientation
  def place(polyomino, aspect)
    return Placement.new(polyomino, aspect, 0) if placements.empty?
    new_place = Placement.new(polyomino, aspect, placements.last&.offset)
    while placements.last.oriented.zip(new_place.oriented).any?{ |row_l, row_r|
      row_l[new_place.offset - placements.last.offset ..].zip(row_r).any?{_1 == 1 && _2 == 1}
    }
      new_place.offset += 1
    end
    new_place
  end

  def render
    # ,_,
    # |_|
    #

    r = Array.new(height + 1){" " * (2 * width + 1)}
    placements.each do |placement|
      po = placement.oriented
      po.each_index do |ri|
        po[ri].each_index do |ci|
          if po[ri][ci] == 1
            r[ri][2 * (ci + placement.offset) + 2] = ","
            r[ri + 1][2 * (ci + placement.offset)] = ","
            r[ri + 1][2 * (ci + placement.offset) + 2] = ","
            if ri > 0 && ci > 0 &&
                po[ri - 1][ci - 1] == 1 && po[ri - 1][ci] == 1 && po[ri][ci - 1] == 1
              r[ri][2 * (ci + placement.offset)] = " "
            else
              r[ri][2 * (ci + placement.offset)] = ","
            end
          end
        end
      end
    end
    placements.each do |placement|
      po = placement.oriented
      po.each_index do |ri|
        po[ri].each_index do |ci|
          if po[ri][ci] == 1
            r[ri][2 * (ci + placement.offset) + 1] = "_" if ri == 0 || po[ri - 1][ci] != 1
            r[ri + 1][2 * (ci + placement.offset)] = "|" if ci == 0 || po[ri][ci - 1] != 1
            r[ri + 1][2 * (ci + placement.offset) + 1] = "_" if po[ri + 1].nil? || po[ri + 1][ci] != 1
            r[ri + 1][2 * (ci + placement.offset) + 2] = "|" if po[ri][ci + 1] != 1
          end
        end
      end
    end
    r
  end
end

if __FILE__ == $0
  polyominoes = gen_polyominoes(ARGV[1].to_i)
    .sort_by{|polyomino| [polyomino.height, polyomino.width, polyomino.to_s]}
    .group_by(&:height).values
  case ARGV[0]
  when "gen"
    polyominoes.each{puts _1.to_s}
    puts "#{polyominoes.length} polyominoes"
  when "naive"
    puts
    width = ARGV[2] || (IO.console.winsize[1] - 1) / 2
    polyominoes.each do |group|
      strip = Strip.new
      group.each do |polyomino|
        placement = strip.place(polyomino, 0)
        if placement.right > width
          puts [strip.bitmasks, strip.render, ""]
          strip = Strip.new
          placement = strip.place(polyomino, 0)
        end
        strip.placements << placement
      end
    puts [strip.bitmasks, strip.render, ""]
    end
  when "help"
    puts <<END
gen (size) - only list polyominoes of a given size
naive (size) (width) - pack polyominoes in lexicographical order according to their bitmask code
help - print this message
END
  else puts 'unknown method; use "gen" or "naive" or type "help" for detailed descriptions'
  end
end
