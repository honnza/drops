require "io/console"

Cell = Struct.new(
  :r, :g, :b, # floats going from 0.0 to 1.0
  :dr, :dg, :db, # accumulated difference from applied forces
  :fixed, # fixed cells are immune to forces
  :join_e, :join_s, # false for repulsive force, true for attractive force
)

Grid = Struct.new(
  :f_join, # force multiplier for the attractive force
  :f_split, # force multiplier for the repulsive force
  :f_temp, # force multiplier for the random noise
  :cells, # grid of Cell elements
) do
  def relax_tick
    cells.each do |row|
      row.each do |cell|
        cell.dr = rand (-f_temp .. f_temp)
        cell.dg = rand (-f_temp .. f_temp)
        cell.db = rand (-f_temp .. f_temp)
      end
    end
    cells.each do |row|
      row.each_cons(2) do |cell_w, cell_e|
        unless cell_w.fixed || cell_e.fixed
          dr = cell_w.r - cell_e.r
          dg = cell_w.g - cell_e.g
          db = cell_w.b - cell_e.b
          d2 = dr ** 2 + dg ** 2 + db ** 2
          if cell_w.join_e
            f_mult = - d2 ** 0.5 * f_join
          else
            f_mult = d2 ** -0.5 * f_split
          end
          cell_w.dr += dr * f_mult
          cell_w.dg += dg * f_mult
          cell_w.db += db * f_mult
          cell_e.dr -= dr * f_mult
          cell_e.dg -= dg * f_mult
          cell_e.db -= db * f_mult
        end
      end
    end
    cells.each_cons(2) do |row_n, row_s|
      row_n.zip row_s do |cell_n, cell_s|
        unless cell_n.fixed || cell_s.fixed
          dr = cell_n.r - cell_s.r
          dg = cell_n.g - cell_s.g
          db = cell_n.b - cell_s.b
          d2 = dr ** 2 + dg ** 2 + db ** 2
          if cell_n.join_s
            f_mult = - d2 ** 0.5 * f_join
          else
            f_mult = d2 ** -0.5 * f_split
          end
          cell_n.dr += dr * f_mult
          cell_n.dg += dg * f_mult
          cell_n.db += db * f_mult
          cell_s.dr -= dr * f_mult
          cell_s.dg -= dg * f_mult
          cell_s.db -= db * f_mult
        end
      end
    end
    cells.each do |row|
      row.each do |cell|
        unless cell.fixed
          cell.r = cell.r + cell.dr
          cell.g = cell.g + cell.dg
          cell.b = cell.b + cell.db
          d2 = (cell.r - 0.5) ** 2 + (cell.g - 0.5) ** 2 + (cell.b - 0.5) ** 2
          if d2.nan?
            cell.r = rand
            cell.g = rand
            cell.b = rand
          else
            rescale = (0.5 / d2 ** 0.5).clamp(.. 1.0)
            cell.r = (cell.r - 0.5) * rescale + 0.5
            cell.g = (cell.g - 0.5) * rescale + 0.5
            cell.b = (cell.b - 0.5) * rescale + 0.5
          end
        end
      end
    end
  end

  def row_headers; (1..).take(cells.length).map{"%2d" % _1}.reverse; end
  def col_headers; (?a..).take(cells[0].length).map{"%2s" % _1}; end

  def render_1x
    print "  " + col_headers.join + "\r\n"
    row_headers.zip cells do |header, row|
      print header
      row.each do |cell|
        r = (cell.r ** 0.45 * 255.0).round
        g = (cell.g ** 0.45 * 255.0).round
        b = (cell.b ** 0.45 * 255.0).round
        print "\e[48;2;%d;%d;%dm  \e[0m" % [r, g, b]
      end
      print header + "\r\n"
    end
    print "  " + col_headers.join + "\r\n"
  end

  def fuse_region(x1, y1)
    region = [[x1, y1]]
    (0..).each do |i|
      break if region.size == i
      x, y = region[i]
      region |= [[x + 1, y]] if x < cells[y].length && cells[y][x].join_e
      region |= [[x, y + 1]] if y < cells.length && cells[y][x].join_s
      region |= [[x - 1, y]] if x > 0 && cells[y][x - 1].join_e
      region |= [[x, y - 1]] if y > 0 && cells[y - 1][x].join_s
    end

    region.each do |x, y|
      cells[y][x].join_e = true if region.include? [x + 1, y]
      cells[y][x].join_s = true if region.include? [x, y + 1]
    end
  end
end

def Grid.checker(f_join, f_split, f_temp, w, h)
  cells = Array.new(h) do
    Array.new(w) do
      Cell.new(
        rand, rand, rand,
        0, 0, 0,
        false, false, false
      )
    end
  end
  Grid.new(f_join, f_split, f_temp, cells)
end

if __FILE__ == $0
  STDIN.raw do
    IO.console.clear_screen
    print "\e[?25l"
    grid = Grid.checker(0.1, 0.002, 0.001, 4, 4)
    input = ""
    loop do
      grid.relax_tick
      IO.console.cursor = [0, 0]
      grid.render_1x
      puts "> #{input}\e[7m \e[0m"

      byte = STDIN.read_nonblock 1, exception: false
      case byte
      when :wait_readable then nil
      when "\x7F"
        IO.console.clear_screen
        input[-1] = "" unless input.empty?
      when "\x03", "\x1A"
        print "\e[?25h"
        exit
      when "\r"
        case input
        when /^new ([0-9]{1,2})x([0-9]{1,2})$/
          IO.console.clear_screen
          grid = Grid.checker(0.1, 0.002, 0.001, $1.to_i, $2.to_i)
        when /^(join|merge|split) ([a-z])([0-9]{1,2}) ((?:\d?[neswurdl^>v<])*)$/
          arg = $1 != "split"
          merge = $1 == "merge"
          x = $2.ord - "a".ord
          y = grid.cells.length - $3.to_i
          grid.cells[y][x].fixed = false
          $4.scan(/(\d*)(\D)/).each do |count, dir|
            (count.empty? ? 1 : count.to_i).times do
              case dir
              when "n", "u", "^"
                y -= 1
                grid.cells[y][x].join_s = arg
              when "e", "r", ">"
                grid.cells[y][x].join_e = arg
                x += 1
              when "s", "d", "v"
                grid.cells[y][x].join_s = arg
                y += 1
              when "w", "l", "<"
                x -= 1
                grid.cells[y][x].join_e = arg
              end
            end
          end
          grid.cells[y][x].fixed = false
          grid.fuse_region x, y if merge
        when /^fix ([a-z])([0-9]{1,2})(?: (\d(?:\.\d+)?) (\d(?:\.\d+)?) (\d(?:\.\d+)?))?$/
          x = $1.ord - "a".ord
          y = grid.cells.length - $2.to_i

          region = [[x, y]]
          (0..).each do |i|
            break if region.size == i
            x, y = region[i]
            region |= [[x + 1, y]] if x < grid.cells[y].length && grid.cells[y][x].join_e
            region |= [[x, y + 1]] if y < grid.cells.length && grid.cells[y][x].join_s
            region |= [[x - 1, y]] if x > 0 && grid.cells[y][x - 1].join_e
            region |= [[x, y - 1]] if y > 0 && grid.cells[y - 1][x].join_s
          end

          region.each do |x, y|
            cell = grid.cells[y][x]
            cell.fixed = true
            cell.r = $3&.to_f || 0
            cell.g = $4&.to_f || 0
            cell.b = $5&.to_f || 0
          end

        when /^upscale( \d+)?$/
          scale = $1.nil? ? 2 : $1.strip.to_i
          grid.cells = grid.cells.flat_map{[_1] * scale}
          grid.cells.map!{|row| row.flat_map{[_1] * scale}}
          grid.cells.each_index do |y|
            grid.cells[y].each_index do |x|
              grid.cells[y][x] = grid.cells[y][x].dup
              grid.cells[y][x].join_e = true unless x % scale == scale - 1
              grid.cells[y][x].join_s = true unless y % scale == scale - 1
            end
          end
        when /^shuffle$/
          grid.cells.each{|row| row.each{unless _1.fixed; _1.r = rand; _1.g = rand; _1.b = rand; end}}
        else
          puts "unknown command"
        end
        IO.console.clear_screen
        input = ""
      else input += byte
      end
    end
  end
end
