require 'io/console'
require 'Singleton'

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
  #fudging vertices by 0.1 causes visible snapping in some causes
  #fudging by 1e-6 causes lines to extend far off-screen and cause performance issues.
  EPSILON = 0.001
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
    # todo: Bressenham's line algorithm
    length = [(x1-x2).abs, (y1-y2).abs, 1].max
    dx = (x1-x2).fdiv length
    dy = (y1-y2).fdiv length
    # p [x1, y1, x2, y2] if length > 1000
    (0..length).each do |i|
      x = (x2+i*dx).round
      y = (y2+i*dy).round
      buffer[y][x] = "#" if x >= x_min && y >= 0 && x <= x_max && y < @height
    end
  end

  def clr_scr; puts "\e[H\e[2J"; end
  def reset_cursor; puts "\e[H"; end

  def render_3d(tile, dir, x_off = 0, y_off = 0)
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
        #fudge for speed
        cam_z = EPSILON * (cam_z > 0 ? 1 : -1) if cam_z > -EPSILON && cam_z < EPSILON
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
        next if corners[cix_r][0] < scan_l && corners[cix_r][1] > 0
        next if corners[cix_l][0] > scan_r && corners[cix_l][1] > 0
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

    recurse[tile, -x_off, -y_off, 0, @width - 1]
    puts buffer
  end
end

#the Player class represents the player's tile, facing direction and position within tile
class Player
  HALF_WIDTH = 0.45
  def initialize(tile, dir, x_off = 0, y_off = 0)
    @tile = tile
    @dir = dir
    @x_off = x_off
    @y_off = y_off
  end
  attr_accessor :tile, :dir, :x_off, :y_off
  
  #translate the player position in the forward direction if the terrain permits
  def walk(dist)
    @x_off += Math.cos(dir) * dist
    @y_off += Math.sin(dir) * dist
    fix_position
  end
  
  #translate the player position perpendicular to the forward direction if the terrain permits
  def strafe(dist)
    @x_off += Math.sin(dir) * dist
    @y_off -= Math.cos(dir) * dist
    fix_position
  end
  
  #rotates the player by a specified amount
  def turn(angle)
    @dir = (@dir + angle) % (2 * Math::PI)
  end
  
  private def fix_position
    @x_off =  HALF_WIDTH if @x_off >  HALF_WIDTH && @tile.e == :wall
    @y_off =  HALF_WIDTH if @y_off >  HALF_WIDTH && @tile.n == :wall
    @x_off = -HALF_WIDTH if @x_off < -HALF_WIDTH && @tile.w == :wall
    @y_off = -HALF_WIDTH if @y_off < -HALF_WIDTH && @tile.s == :wall
    #todo: fix corners
    
    (@x_off -= 1; @tile = @tile.e) if @x_off >  0.5
    (@y_off -= 1; @tile = @tile.n) if @y_off >  0.5
    (@x_off += 1; @tile = @tile.w) if @x_off < -0.5
    (@y_off += 1; @tile = @tile.s) if @y_off < -0.5
  end
end

class GameController
  def initialize(player)
    @last_time = Time.now
    @player = player
        
    scr_size = IO.console.winsize[1]
    render_size = scr_size % 4 == 0 ? scr_size - 3 : scr_size - scr_size % 4 + 1
    @renderer = Renderer.new(render_size, render_size / 2 + 1)
  end
  
  def key_pressed(*keys)
    keys.any?{|key| IO.console.pressed? key.ord}
  end

  def frame_time
    new_time = Time.now
    delta = new_time - @last_time
    @last_time = new_time
    delta
  end

  def run_smooth_kbd
    @renderer.clr_scr
    loop do
      frame_time = self.frame_time
      
      @player.walk( frame_time)   if key_pressed "W", 38, 104
      @player.walk(-frame_time)   if key_pressed "S", 40, 98, 101
      @player.strafe(-frame_time) if key_pressed "A", 100
      @player.strafe( frame_time) if key_pressed "D", 102
      @player.turn( frame_time)   if key_pressed "Q", 37, 103
      @player.turn(-frame_time)   if key_pressed "E", 39, 105
      
      return if key_pressed 27, 32
      
      @renderer.reset_cursor
      @renderer.render_3d @player.tile, @player.dir, @player.x_off, @player.y_off
      sleep 0.016
    end
  end
end

maze = gen_maze 10, 10
tile = maze.sample
player = Player.new(tile, 0)

GameController.new(player).run_smooth_kbd
