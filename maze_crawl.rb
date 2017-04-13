require 'io/console'
require 'singleton'

#A space with four sides, each of which can be either a :wall, or another Tile
#tile.n == :wall || tile.n.s, and similarly for all other sides.
Tile = Struct.new :id, :n, :e, :s, :w, :decorations do
  def initialize(id, n=:wall, e=:wall, s=:wall, w=:wall, decorations=[]); super; end
  def wall?(dir); self[dir] == :wall; end
  def walls; %i{n e s w}.select{|k| wall? k}; end
end


def gen_maze(width, height, debug: false)
  tiles = Array.new(height){|y| Array.new(width){|x| Tile.new [x, y]}}
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

module RoomDecoration
  attr_reader :lines
  def rotate_90
    @lines.map!{|line|[-line[1], line[0], line[2], -line[4], line[3], line[5]]}
  end
  
  def rotate_to_wall(wall)
    %i{n w s e}.find_index(wall).times{rotate_90}
  end
  
  def rotate_to_corridor_end(tile)
    rotate_to_wall case tile.walls
    when [:n, :e, :s] then :e
    when [:n, :e, :w] then :n
    when [:n, :s, :w] then :w
    when [:e, :s, :w] then :s
    else raise ArgumentError, "not a corridor end: #{tile.walls}"
    end
  end
end

class DoorRD
  include RoomDecoration
  def initialize(where)
    @lines = [
      [-0.25, 0.5,  1  , -0.25, 0.5, -0.5],
      [-0.25, 0.5, -0.5,  0.25, 0.5, -0.5],
      [ 0.25, 0.5, -0.5,  0.25, 0.5,  1  ]
    ]
    where.is_a?(Symbol) ? rotate_to_wall(where) : rotate_to_corridor_end(where)
  end
end

def gen_door(maze)
  loop do
    tile = maze.sample
    redo if tile.walls.count != 3
    redo if tile.decorations.any?{|d|d.is_a? DoorRD}
    tile.decorations << DoorRD.new(tile)
    return tile
  end
end

class IO
  def has_data?
    #source: http://stackoverflow.com/a/948077/
    result = IO.select([self], nil, nil, 0)
    result && (result.first.first == self)
  end
  
  def getch_nonblock
    getch if has_data?
  end
end

class Renderer
  #fudging vertices by 0.1 causes visible snapping in some causes
  #fudging by 1e-6 causes lines to extend far off-screen and cause performance issues.
  #todo: truncate lines while rendering instead.
  EPSILON = 0.001
  #the default values for ceil and floor account for character aspect ratio
  def initialize width, height, hfov = 120
    @width = width
    @height = height
    @scale = (@width / 2) / Math.tan(hfov * (Math::PI / 180) / 2)
    @ceil = 0.25
    @floor = 0.25
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
      
      tile.decorations.each do |dec|
        p(dec.lines).each do |line|
          cam_x1 = - (line[1] + y_off) * cos_dir + (line[0] + x_off) * sin_dir
          cam_y1 = line[2] * @ceil
          cam_z1 = + (line[1] + y_off) * sin_dir + (line[0] + x_off) * cos_dir
          cam_x2 = - (line[4] + y_off) * cos_dir + (line[3] + x_off) * sin_dir
          cam_y2 = line[5] * @ceil
          cam_z2 = + (line[4] + y_off) * sin_dir + (line[3] + x_off) * cos_dir
          px_x1 = ((cam_x1 / cam_z1 * @scale + 0.5) + @width / 2).round
          px_y1 = ((cam_y1 / cam_z1 * @scale + 0.5) + @height / 2).round
          px_x2 = ((cam_x2 / cam_z2 * @scale + 0.5) + @width / 2).round
          px_y2 = ((cam_y2 / cam_z2 * @scale + 0.5) + @height / 2).round
          
          render_line px_x1, px_y1, px_x2, px_y2, scan_l, scan_r, buffer
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
    @x_off =  HALF_WIDTH if @x_off >  HALF_WIDTH && @tile.wall?(:e)
    @y_off =  HALF_WIDTH if @y_off >  HALF_WIDTH && @tile.wall?(:n)
    @x_off = -HALF_WIDTH if @x_off < -HALF_WIDTH && @tile.wall?(:w)
    @y_off = -HALF_WIDTH if @y_off < -HALF_WIDTH && @tile.wall?(:s)
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
    @getch_fallback = false

    scr_size = IO.console.winsize[1]
    render_size = scr_size % 4 == 0 ? scr_size - 3 : scr_size - scr_size % 4 + 1
    @renderer = Renderer.new(render_size, render_size / 2 + 1)
  end

  def scan_keys
    @last_key = IO.console.getch_nonblock&.upcase
  end

  def key_pressed(*keys)
    if @getch_fallback
      keys.include? @last_key
    else
      keys.any?{|key| IO.console.pressed? key.ord}
    end
  rescue NotImplementedError #this is what's raised on Macs.
    @getch_fallback = true
    retry
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

      scan_keys
      @player.walk( frame_time)   if key_pressed "W", "8", 38, 104
      @player.walk(-frame_time)   if key_pressed "S", "2", "5", 40, 98, 101
      @player.strafe(-frame_time) if key_pressed "A", "4", 100
      @player.strafe( frame_time) if key_pressed "D", "6", 102
      @player.turn( frame_time)   if key_pressed "Q", "7", 37, 103
      @player.turn(-frame_time)   if key_pressed "E", "9", 39, 105

      return if key_pressed 27, " "

      @renderer.reset_cursor
      @renderer.render_3d @player.tile, @player.dir, @player.x_off, @player.y_off
      sleep 0.016
    end
  end
end

maze = gen_maze 5, 5
start = gen_door maze
goal  = gen_door maze
player = Player.new(start, 0)

GameController.new(player).run_smooth_kbd
