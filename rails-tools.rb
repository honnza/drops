require "matrix"

def gen_hypercube(total, &op)
  r = [rand(total)]
  until r.size >= total
    gen = rand(total)
    r |= r.map{|x| op.(x, gen)}
    p [gen, r.size]
  end
  r
end

def gen_group(first, gens, &op)
  gen_at = gens.map{0}
  elems = [first]
  loop do
    (0...gens.count).find do |gen_ix|
      new_elem = op.(elems[gen_at[gen_ix]], gens[gen_ix])
      unless elems.include?(new_elem)
        elems << new_elem
        gen_at[gen_ix] += 1
      end
    end or return elems
  end
end

def gen_lattice(dims, first = nil, gens = [])
  gens_at = []
  gen_stats = []
  elems = [first || dims.map{|d| rand d}]
  loop do
    gen_ix = gens_at.find_index{|gen_at| gen_at < elems.count}
    if gen_ix
      new_elem = elems[gens_at[gen_ix]]
        .zip(gens[gen_ix]).map{|x, y| x + y}
      if new_elem.zip(dims).all? {|e, d| (0...d).include? e} && !elems.include?(new_elem)
        elems |= [new_elem]
        gen_stats[gen_ix] += 1
        if elems.count == dims.reduce(&:*)
          p gens.zip gen_stats
          return elems
        end
      end
      gens_at[gen_ix] += 1
    else
      gens << dims.map{|d| rand (-d+1)..(d-1)}
      gens_at << 0
      gen_stats << 0
    end
  end
end

def digitwise_sum(b)
  f = Proc.new{|x, y| (x + y) % b + (x < b && y < b ? 0 : f[x/b, y/b] * b)}
end

def gen_symmetric(dims, &orbit_f)
  dims.map{(0 ... _1).to_a}.reduce(&:product).map(&:flatten)
      .group_by(&orbit_f).values.shuffle.map(&:shuffle).flatten(1)
end

# x = gen_group(rand(144), x=[*0..143].shuffle, &digitwise_sum(12))

# #x=[*0..99].shuffle; x.each_slice(20).map{|x| puts x.join " "}
# puts x.map{|x|(x/12).to_s(12)}.join; puts x.map{|x|(x%12).to_s(12)}.join
# #x.map{|e|puts e;g=gets; x<< g+e.to_s if g[/./]}

# h="";loop{h+=rand(2).to_s;print h;h="" if gets[/./]}

def sample_smooth(w, h)
  pts = [*0...w].product [*0...(h||w)]
  pts.select!{_1 <= _2} if h.nil?
  visited_x = [0] * w
  visited_y = h.nil? ? visited_x : ([0] * h)
  pts.shuffle!
  Enumerator.new do |e|
    loop do
      x, y = pts.min_by{|x, y| [visited_x[x], visited_y[y]]}
      e.yield [[x, y], [visited_x[x], visited_y[y]]]
      visited_x[x] += 1
      visited_y[y] += 1
      pts.delete [x, y]
    end
  end
end

class Rational
  def to_mixed_s
    if denominator == 1
      "#{self < 0 ? ?- : ""}#{to_i.abs}r"
    else
      "#{self < 0 ? ?- : ""}#{to_i.abs} #{(self-to_i).numerator.abs}/#{denominator}"
    end
  end
  def gcd(other); self.numerator.gcd(other.numerator).to_r / self.denominator.lcm(other.denominator); end
end

class Numeric
  def round_toward(other); round(half: (self > other) ^ (self < 0) ? :down : :up); end
  def round_away(other); round(half: (self < other) ^ (self < 0) ? :down : :up); end
end

# math yanked from https://en.m.wikipedia.org/wiki/Delaunay_triangulation 
# and https://en.m.wikipedia.org/wiki/Circumscribed_circle#Triangles
class Triangle
  def initialize(*pts, z_metric:)
    @pts = pts.map{|pt| Vector[pt[0], pt[1], 0]}
    @es = [@pts[2] - @pts[1], @pts[0] - @pts[2],  @pts[1] - @pts[0]]
    @pts_paraboloid = @pts.map{|pt| Vector[pt[0], pt[1], pt.dot(pt)]}
    @ns = pts.map{|pt| pt[2]}
    z_gap = [
      z_metric[@ns[0], @ns[1]],
      z_metric[@ns[1], @ns[2]],
      z_metric[@ns[2], @ns[0]]
    ].max
    @tonedown = case
                when z_gap > z_metric[@ns[2], @ns[0]] && z_gap > z_metric[@ns[0], @ns[1]] then 0
                when z_gap > z_metric[@ns[0], @ns[1]] && z_gap > z_metric[@ns[1], @ns[2]] then 1
                when z_gap > z_metric[@ns[1], @ns[2]] && z_gap > z_metric[@ns[2], @ns[0]] then 2
                else nil
                end
    @priority = [ # not 1x1; maximize gradient; minimize perimeter; top to bottom
      (@es[0].dot(@es[0]) < 3 && @es[1].dot(@es[1]) < 3 && @es[2].dot(@es[2]) < 3) ? 0 : 1,
      z_gap,  
      (z_gap > 0 ? -1 : 1) * @es.map(&:norm).sum,
      pts.sort.flatten.map(&:-@)
    ]
    puts self
    (puts "triangle rejected"; raise ArgumentError) if @es[1].cross(@es[0])[2] <= 0
  end

  def pts; @pts.zip(@ns).map{|pt, n| [pt[0], pt[1], n]}; end
  def reject; @priority[0] = 0; end
  def rejected?; @priority[0] == 0; end
  def z_gap; @priority[1]; end
  attr_reader :priority

  def include?(x, y)
    pt = Vector[x, y, 0]
    (pt - @pts[0]).cross(@pts[1] - @pts[0])[2] >= 0 &&
    (pt - @pts[1]).cross(@pts[2] - @pts[1])[2] >= 0 &&
    (pt - @pts[2]).cross(@pts[0] - @pts[2])[2] >= 0
  end

  def circumcircle_include?(x, y)
    xyp = Vector[x, y, x**2 + y**2]
    Matrix::LUPDecomposition.new(Matrix.rows(@pts_paraboloid.map{|pt| (xyp - pt).to_a})).det >= 0
  end
  
  def centroid; (@pts.inject(&:+) / 3r).to_a; end

  def bounding_center
    pts = @pts.map{|pt| pt.map &:to_r}
    pt = case
         when @es[0].dot(@es[1]) >= 0 then (pts[0] + pts[1]) / 2
         when @es[1].dot(@es[2]) >= 0 then (pts[1] + pts[2]) / 2
         when @es[2].dot(@es[0]) >= 0 then (pts[2] + pts[0]) / 2
         else
            l0_2 = @es[0].dot @es[0]
            l1_2 = @es[1].dot @es[1]
            l2_2 = @es[2].dot @es[2]
            w0 = l0_2 * (l1_2 + l2_2 - l0_2)
            w1 = l1_2 * (l2_2 + l0_2 - l1_2)
            w2 = l2_2 * (l0_2 + l1_2 - l2_2)
            pt = (w0 * pts[0] + w1 * pts[1] + w2 * pts[2]) / (w0 + w1 + w2)
            gcd = @pts.flat_map{|pt2| [pt[0] - pt2[0], pt[1] - pt2[1]]}.reduce(0r, &:gcd)
            xys = @pts.map do |pt2|
              [0, 1].map{|ix| (pt[ix] - pt2[ix]).abs / gcd}.sort
            end.sort.uniq
            puts xys.map{|x, y| "#{x.to_mixed_s}^2 + #{y.to_mixed_s}^2"}.join(" = ")
                    .gsub(/0r\^2 + \| \+ 0r\^2/, "")
            pt
         end
    [pt[0], pt[1]]
  end

  def to_s
    "T[#{"\e[30;1m" if @tonedown == 0}#{pts[0]}\e[0m, #{
         "\e[30;1m" if @tonedown == 1}#{pts[1]}\e[0m, #{
         "\e[30;1m" if @tonedown == 2}#{pts[2]}\e[0m]"
  end
  def inspect; to_s; end
end


def voronoi_subdivide(xs, ys, reflexive = false, z_metric: -> a, b {(a - b).abs})

  z_metric = case z_metric
             when :discrete then -> x, y {x == y ? 0 : 1}
             when :linear then -> x, y {(x-y).abs}
             when :rgb then lambda do |x, y|
               a, b, c, *_ = x.digits(1000) + [0, 0]
               d, e, f, *_ = y.digits(1000) + [0, 0]
               (a - d) ** 2 + (b - e) ** 2 + (c - f) ** 2
             end
             when :pcd_rgb then lambda do |x, y|
               a, b, c, *_ = x.digits(1000) + [0, 0]
               d, e, f, *_ = y.digits(1000) + [0, 0]
               rm = (a + d) / 512.0
               (2 + rm) * (a - d) ** 2 + 4 * (b - e) ** 2 + (3 - rm) * (c - f) ** 2
             end
             when :xxyy then lambda do |x, y|
               a, b, c, d, *_ = x.digits(10) + [0, 0, 0]
               e, f, g, h, *_ = y.digits(10) + [0, 0, 0]
               (a + 8 * b - e - 8 * f) ** 2 + (c + 8 * d - g - 8 * h) ** 2
             end
             else z_metric
             end

  p [xs.first, ys.first]
  n_0_0 = gets.to_i
  p [xs.last, ys.first] unless reflexive
  n_1_0 = gets.to_i unless reflexive
  p [xs.first, ys.last]
  n_0_1 = gets.to_i
  p [xs.last, ys.last]
  n_1_1 = gets.to_i
  xl = xs.length - 1
  yl = ys.length - 1
  triangles = [
    Triangle.new([0, 0, n_0_0], [0, yl, n_0_1], [xl, yl, n_1_1], z_metric:),
    (Triangle.new([xl, 0, n_1_0], [0, 0, n_0_0], [xl, yl, n_1_1], z_metric:) unless reflexive)
  ].compact
  lines_by_z_gap = Hash.new{|h, k| h[k] = []}
  
  loop do
    print "\npop "
    t = triangles.max_by(&:priority)
    return if t.rejected?
    x, y = nil
    if lines_by_z_gap.empty? || t.z_gap > lines_by_z_gap.keys.max
      p t
      cx, cy = t.bounding_center
      ox, oy = t.centroid
      x = cx.round_away ox
      y = cy.round_away oy
      op = "<-"
      if t.pts.any?{|px, py, _| px == x && py == y} || x > y && reflexive
        x = cx.round_toward ox
        y = cy.round_toward oy
        op = "->"
      end
      if triangles.any?{|t| t.pts.any?{|px, py, _| px == x && py == y}}
        t.reject
        puts "rejected"
        next
      end
      puts case
      when cx.denominator != 2 && cy.denominator != 2
        "[#{cx.to_mixed_s}, #{cy.to_mixed_s}] | [#{ox.to_mixed_s}, #{oy.to_mixed_s}]"
      when op == "->"
        "[#{cx.to_mixed_s}, #{cy.to_mixed_s}] -> [#{ox.to_mixed_s}, #{oy.to_mixed_s}]"
      else
        "<- [#{cx.to_mixed_s}, #{cy.to_mixed_s}] | [#{ox.to_mixed_s}, #{oy.to_mixed_s}]"
      end
    else
      line = p lines_by_z_gap[lines_by_z_gap.keys.max].min
      if (line[0][0] - line[1][0]) ** 2 + (line[0][1] - line[1][1]) ** 2 == 4
        x = (line[0][0] + line[1][0]) / 2
        y = (line[0][1] + line[1][1]) / 2
        x, y = nil if triangles.any?{|t| t.pts.any?{|px, py| px == x && py == y}}
      else
        x, y = ([line[0][0], line[1][0]].max - 1 .. [line[0][0], line[1][0]].min + 1).to_a
          .product(([line[0][1], line[1][1]].max - 1 .. [line[0][1], line[1][1]].min + 1).to_a)
          .find do |x, y|
            x >= 0 && x <= xl && y >= 0 && y <= yl && (x <= y || !reflexive) &&
              !triangles.any?{|t| t.pts.any?{|px, py| px == x && py == y}}
          end
      end
      if x.nil?
        lines_by_z_gap.each_value{|ls| ls.delete line}
        lines_by_z_gap.reject!{|_, v| v.empty?}
        next
      end
    end
    p x, y
    p [xs[x], ys[y]]

    n = nil
    loop do
      case gets
      when /^(\d+)$/ then n = $1.to_i
      when /^(\d+) (\d+) (\d+)$/
        x = $1.to_i; y = $2.to_i; n = $3.to_i
        lines_by_z_gap.each_value do |ls|
          ls.reject!{|pt1, pt2| pt1[0] == x && pt1[1] == y || pt2[0] == x && pt2[1] == y}
        end
        lines_by_z_gap.reject!{|_, v| v.empty?}
      else redo
      end
      break
    end

    ts = triangles.select{|t| t.circumcircle_include?(x, y)}
    triangles.reject!{|t| ts.include? t}
    ts.each{|t| puts ?- + t.to_s}
    pts = p ts.flat_map(&:pts).uniq.sort_by{|px, py, _| Math.atan2(px - x, py - y)}.reject{|px, py| px == x && py == y}
    triangles += (pts + [pts.first]).each_cons(2).map{|pt1, pt2| Triangle.new(pt1, pt2, [x, y, n], z_metric:) rescue nil}.compact
    pts.each do |pt|
      if (pt[0] - x).abs <= 2 && (pt[1] - y).abs <= 2
        lines_by_z_gap[z_metric[pt[2], n]] << p([pt, [x, y, n]].sort) 
      end
    end
  end
end

if __FILE__ == $0
  case ARGV[0]
  when "gen"
    md = %r{^(?:(?:(\d+)(?:x(\d+))?)?(?:/(?:(\d+)x)?(\d+))?)?$}.match(ARGV[2] || "")
    if md.nil?
      puts "can't parse #{ARGV[2]} as a (w)x(h)/(bh)x(bw) string"
      exit
    end
    w, h, bh, bw = md[1..].to_a.map{_1&.to_i}
    w ||= 10
    h ||= w
    bh ||= w * h
    bw ||= w

    pts = case ARGV[1]
          when "lattice" then gen_lattice [w, h]
          when "shuffle" then [*0...w].product([*0...h]).shuffle
          when "hypercube", "smooth"
            puts "TODO"
            exit
          else
            puts "#{ARGV[1]} isn't a recognized generation method"
            exit
          end

    pts.each_slice(bw * bh) do |block|
      block.each_slice(bw) do |row|
        puts row.map{_1.join(w <= 10 && h <= 10 ? "" : ",")}.join(" ")
      end
      puts
    end
  when "sample"
    case ARGV[1]
    when "shuffle"
      md = %r{^(\d+)(?:x(\d+))?$}.match(ARGV[2])
      if md.nil?
        puts "can't parse #{ARGV[2]} as a (w)x(h) string"
        exit
      end
      w, h = md[1..].to_a.map{_1&.to_i}
      pts = [*0...w].product [*0...(h||w)]
      pts.select!{_1 <= _2} if h.nil?
      ARGV.replace([])
      pts.shuffle!.each{print _1; gets}
    when "smooth"
      md = %r{^(\d+)(?:x(\d+))?$}.match(ARGV[2])
      if md.nil?
        puts "can't parse #{ARGV[2]} as a (w)x(h) string"
        exit
      end
      w, h = md[1..].to_a.map{_1&.to_i}
      ARGV.replace([])
      sample_smooth(w, h).each{print _1; gets}
    when "voronoi"
      md = %r{^(\d+)(?:x(\d+))?$}.match(ARGV[3])
      if md.nil?
        puts "can't parse #{ARGV[3]} as a (w)x(h) string"
        exit
      end
      w, h = md[1..].to_a.map{_1&.to_i}
      unless %w{discrete linear pcd_rgb rgb xxyy}.include?(ARGV[2])
        puts "#{ARGV[2]} isn't a recognized z metric method"
        exit
      end
      z_metric = ARGV[2].to_sym
      ARGV.replace([])
      voronoi_subdivide [*0...w], [*0...(h||w)], h.nil?, z_metric:
    end
  else
    puts <<END
gen (method) (w)x(h)/(bh)x(bw) - shuffle a (bw)x(bh) square grid, then prints the points (bw) points in a row, (bh) rows in a block. If unspecified, (h) defaults to (w), (w) defaults to 10, (bh) defaults to (w)x(h) (only one block), and (bw) defaults to (w).
    shuffle - pick points in a completely random order
    hypercube - start with a random point. Select a random vector and add a copy of all points moved by that vector. Repeat until all points have been selected.
    lattice - start with a random point. Shuffle the list of offsets. Move every point by every offset prioritizing earlier offsets until all points have been selected.
    smooth - like lattice, but if a later offset was used more than an earlier offset, reorder the offsets and retry.

sample voronoi (method) (w)x(h) - start at the four corners of a (w)x(h) rectangle. Prompt the z value for each. Repeatedly pick and prompt points in the middle of the greatest z gap between nearby points. If height is not specifiecd, start at the three corners of a (w)x(w) upper diagonal triagle instead.
    discrete - any two distinct z values are considered equally distinct
    linear - z values are interpreted as decimal values on a linear scale.
    rgb - z values are interpreted as triples of three-digit numbers and their distance is measured using the Euclidean metric.
    pcd_rgb - z values arre interpreted as RGB triplets, but human perception is taken into account when determining distance.
    xxyy - zvalues are interpreted as pairs of two-digit numbers in base 8 and their distance is measured using the Euclidean metric.
sample shuffle (w)x(h) - all pairs of a (w)x(h) rectangle / (w)x(w) triangle are taken in random order
sample smooth (w)x(h) - pairs are taken in random order, but priority is taken to endpoints that have been used fewer times already

END
  end
end
