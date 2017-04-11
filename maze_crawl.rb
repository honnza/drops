require 'io/console'

#A space with four sides, each of which can be either a :wall, or another Tile
#tile.n == :wall || tile.n.s, and similarly for all other sides.
Tile = Struct.new :id, :n, :e, :s, :w

def gen_maze(width, height, debug: false)
  tiles = Array.new(height){|y| Array.new(width){|x| Tile.new [x, y], :wall, :wall, :wall, :wall}}
  vis_tiles = [[0, 0]]
  vis_edges = [[0, 0, 0, 1, rand], [0, 0, 1, 0, rand]]
  until vis_tiles.size == width * height
    edge = vis_edges.min_by &:last
    vis_edges.delete edge
    ox, oy, nx, ny, _ = edge
    unless vis_tiles.include? [nx, ny]
      case
      when ox == nx && oy < ny
        tiles[oy][ox].s = tiles[ny][nx]
        tiles[ny][nx].n = tiles[oy][ox]
      when ox == nx && oy > ny
        tiles[oy][ox].n = tiles[ny][nx]
        tiles[ny][nx].s = tiles[oy][ox]
      when ox < nx && oy = ny
        tiles[oy][ox].e = tiles[ny][nx]
        tiles[ny][nx].w = tiles[oy][ox]
      when ox > nx && oy = ny
        tiles[oy][ox].w = tiles[ny][nx]
        tiles[ny][nx].e = tiles[oy][ox]
      end
      vis_tiles << [nx, ny]
      vis_edges << [nx, ny, nx, ny + 1, rand] unless ny + 1 == height
      vis_edges << [nx, ny, nx, ny - 1, rand] unless ny - 1 == -1
      vis_edges << [nx, ny, nx + 1, ny, rand] unless nx + 1 == width
      vis_edges << [nx, ny, nx - 1, ny, rand] unless nx - 1 == -1
    end
  end

  if debug
    puts "+" + "--+" * width
    tiles.each do |row|
      puts "|" + row.map{|tile| tile.e == :wall ? "  |" : "   "}.join
      puts "+" + row.map{|tile| tile.s == :wall ? "--+" : "  +"}.join
    end
  end

  tiles.flatten
end

class Renderer
  #the default values for ceil and floor account for character aspect ratio
  def initialize width, height, hfov = 120, ceil = 0.25, floor = ceil
    @width = width
    @height = height
    @scale = (@width / 2) / Math.tan(hfov * (Math::PI / 180) / 2)
    @ceil = ceil
    @floor = floor
  end

  attr_reader :width, :height

  private def render_corner(px_x, px_ceil, px_floor, buffer)
    # p [:c, px_x, px_ceil, px_floor]
    px_x = px_x.round
    px_ceil = 0 if px_ceil < 0
    px_floor = @height - 1 if px_floor >= @height
    (px_ceil .. px_floor).each{|y| buffer[y][px_x] = "|"}
  end

  private def render_wall(pt1, pt2, x_min, x_max, buffer)
    # p [pt1, pt2, x_min, x_max]
    pt1, pt2 = pt2, pt1 if pt1[1] < 0
    pt2 = pt1.zip(pt2).map{|c1, c2| 2 * c1 - c2} if pt2[1] < 0
    render_line pt1[0], pt1[2], pt2[0], pt2[2], x_min, x_max, buffer
    render_line pt1[0], pt1[3], pt2[0], pt2[3], x_min, x_max, buffer
  end
  
  private def render_line(x1, y1, x2, y2, x_min, x_max, buffer)
    # p [x1, y1, x2, y2]
    # todo: Bressenham's line algorithm
    length = [(x1-x2).abs, (y1-y2).abs].max
    dx = (x1-x2).fdiv length
    dy = (y1-y2).fdiv length
    (0..length).each do |i|
      x = (x2+i*dx).round
      y = (y2+i*dy).round
      buffer[y][x] = "#" if x >= x_min && y >= 0 && x <= x_max && y < @height
    end
  end

  def render(tile, dir, x_off = 0, y_off = 0)
    buffer = Array.new(@height){" " * @width}
    sin_dir = Math.sin dir
    cos_dir = Math.cos dir

    recurse = lambda do |tile, x_off, y_off, scan_l, scan_r|
      # p [
      #   tile.id, tile.each_pair.map{|k, v| k if v == :wall}.compact,
      #   x_off, y_off, scan_l, scan_r
      # ]

      corners = []
      [[-0.5, -0.5], [-0.5, 0.5], [0.5, -0.5], [0.5, 0.5]].each do |cx, cy|
        cam_x = - (cy + y_off) * cos_dir + (cx + x_off) * sin_dir
        cam_z = + (cy + y_off) * sin_dir + (cx + x_off) * cos_dir
        px_x = ((cam_x / cam_z * @scale + 0.5) + @width / 2).round
        px_ceil = (@height / 2 - @ceil * @scale / cam_z).round
        px_floor = (@height / 2 + @floor * @scale / cam_z).round
        corners << [px_x, cam_z, px_ceil, px_floor]
        # p [cx, cy]
        render_corner px_x, px_ceil, px_floor, buffer if cam_z > 0 && scan_l < px_x && scan_r > px_x
      end

      [
        ([:n, 1, 3, 0, 1] if y_off > -0.5),
        ([:e, 3, 2, 1, 0] if x_off > -0.5),
        ([:s, 2, 0, 0, -1] if y_off < 0.5),
        ([:w, 0, 1, -1, 0] if x_off < 0.5)
      ].compact.each do |side, cix_l, cix_r, dx, dy|
        # p [side, cix_l, cix_r, dx, dy]
        # next if corners[cix_r][0] < scan_l && corners[cix_r][1] < 0
        # next if corners[cix_l][0] > scan_r && corners[cix_l][1] < 0
        next if corners[cix_l][1] < 0 && corners[cix_r][1] < 0
        if tile[side] == :wall
          render_wall corners[cix_l], corners[cix_r], scan_l + 1, scan_r - 1, buffer
        else
          scan_l_new = [scan_l, (corners[cix_l][0] if corners[cix_l][1] > 0)].compact.max
          scan_r_new = [scan_r, (corners[cix_r][0] if corners[cix_r][1] > 0)].compact.min
          recurse[tile[side], x_off + dx, y_off + dy, scan_l_new, scan_r_new]
        end
      end
    end

    recurse[tile, x_off, y_off, 0, @width - 1]
    puts buffer
  end
end

maze = gen_maze 10, 10
tile = p maze.sample
scr_size = IO.console.winsize[1]
render_size = scr_size % 4 == 0 ? scr_size - 3 : scr_size - scr_size % 4 + 1
renderer = Renderer.new(render_size, render_size / 2 + 1)

angle = 0
x = rand - 0.5
y = rand - 0.5
loop do
  puts "\e[H\e[2J" # move cursor to top left and clear screen
  renderer.render(tile, angle, x, y)
  angle += 0.1
  sleep 0.1
end
