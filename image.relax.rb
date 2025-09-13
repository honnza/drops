require "io/console"
require "chunky_png"

def progress_bar progress, text = "", width = IO.console.winsize[1] - 1
  on_cells = ((width - 2) * progress.clamp(0 .. 1))
  if on_cells > text.length
    text = text.ljust(width - 2)
    text[on_cells.floor] = (0x2590 - on_cells % 1 * 8).floor.chr(Encoding::UTF_8) if on_cells % 1 != 0
    (text + "]").insert(on_cells.floor, "\e[0m")
                .insert(0, "[\e[107;30m")
  else
    bg = (on_cells % 1 * 256).floor
    fg = bg > 127 ? 30 : 97
    (text.ljust(width - 2) + "]")
      .insert(on_cells.floor + 1, "\e[0m")
      .insert(on_cells.floor, "\e[48;2;#{bg};#{bg};#{bg};#{fg}m")
      .insert(0, "[\e[107;30m")
  end
end

def render_fn w, h, &fn
  (0 ... (h + 1)/2).each do |hri|
    (0 ... w).each do |ci|
      top_r, top_g, top_b = fn[hri * 2, ci]
      bot_r, bot_g, bot_b = hri * 2 + 1 == h ? [0, 0, 0] : fn[hri * 2 + 1, ci]
      print "\e[38;2;%d;%d;%d;48;2;%d;%d;%dm\u2580" %
        [top_r, top_g, top_b, bot_r, bot_g, bot_b].map{_1.clamp(0..255)}
    end
    puts "\e[0m"
  end
end

def display_buf w, h, buf, gamma
  igamma = 1 / gamma
  render_fn w, h do |ri, ci|
    buf[3 * (w * ri + ci), 3].map{_1.clamp(0..) ** igamma}
  end
end

def display_diff w, h, buf, gamma
  buf_max = buf.lazy.map{_1.abs}.max
  igamma = 1 / gamma
  render_fn w, h do |ri, ci|
    buf[3 * (w * ri + ci), 3].map{(_1 / buf_max + 1) / 2 ** igamma * 256}
  end
end

wrapping = ARGV.include? "-w"
ARGV.delete "-w"
clamp = ARGV.include? "-c"
ARGV.delete "-c"
gamma = ARGV.find{/-g(\d+(?:\.\d+)?)/ =~ _1}
ARGV.delete gamma
gamma = gamma.nil? ? 1.0 : $1.to_f

if ARGV.length != 2 || gamma == 0.0 || !%w{normal linear}.include?(ARGV[0])
  puts "usage: ruby image-relax [-g1.0] [-w] [method] [filename]"
  puts "-g sets the exponent for gamma correction"
  puts "-w sets wrapping mode on"
  exit
end

img = ChunkyPNG::Canvas.from_file ARGV[1]
mask = img.pixels.map{_1 & 255 > 127}
buf = img.pixels.flat_map do |c|
  if c & 255 > 127
    [c >> 24 & 255, c >> 16 & 255, c >> 8 & 255].map{_1.to_f ** gamma}
  else
    [127 ** gamma] * 3
  end
end
diff_buf = buf.dup


begin
  puts "\e[?25l\e[?1049h"
  (0 ..).each do |t|
    diff_buf.map!{0.0}
    case ARGV[0]
    when "normal"
      (0 ... img.height - (wrapping ? 0 : 1)).each do |ri|
        (0 ... img.width).each do |ci|
          (0 ... 3).each do |ch|
            err = buf[3 * (img.width * ri + ci) + ch] -
                  buf[3 * (img.width * ((ri + 1) % img.height) + ci) + ch]
            diff_buf[3 * (img.width * ri + ci) + ch] -= err
            diff_buf[3 * (img.width * ((ri + 1) % img.height) + ci) + ch] += err
          end
        end
      end
      (0 ... img.height).each do |ri|
        (0 ... img.width - (wrapping ? 0 : 1)).each do |ci|
          (0 ... 3).each do |ch|
            err = buf[3 * (img.width * ri + ci) + ch] -
                  buf[3 * (img.width * ri + ((ci + 1) % img.width)) + ch]
            diff_buf[3 * (img.width * ri + ci) + ch] -= err
            diff_buf[3 * (img.width * ri + ((ci + 1) % img.width)) + ch] += err
          end
        end
      end
    when "linear"
      (0 ... img.height - (wrapping ? 0 : 1)).each do |ri|
        (0 ... img.width - (wrapping ? 0 : 1)).each do |ci|
          (0 ... 3).each do |ch|
            err = buf[3 * (img.width * ri + ci) + ch] -
                  buf[3 * (img.width * ((ri + 1) % img.height) + ci) + ch] -
                  buf[3 * (img.width * ri + ((ci + 1) % img.width)) + ch] +
                  buf[3 * (img.width * ((ri + 1) % img.height) + ((ci + 1) % img.width)) + ch]

            diff_buf[3 * (img.width * ri + ci) + ch] -= err
            diff_buf[3 * (img.width * ((ri + 1) % img.height) + ci) + ch] += err
            diff_buf[3 * (img.width * ri + ((ci + 1) % img.width)) + ch] += err
            diff_buf[3 * (img.width * ((ri + 1) % img.height) + ((ci + 1) % img.width)) + ch] -= err
          end
        end
      end
    end
    max = 255 ** gamma
    max_diff = 0.0
    buf.each_index do
      if mask[_1 / 3]
        diff_buf[_1] = 0
      end
    end
    buf.each_index do
      buf[_1] += diff_buf[_1] / 8
      buf[_1] = buf[_1].clamp(0.0, max) if clamp
      diff_buf[_1] = 0 if buf[_1] == 0 || buf[_1] == max
      max_diff = diff_buf[_1].abs if max_diff < diff_buf[_1].abs
    end
    IO.console.cursor = [0, 0]
    if t % 64 == 0
      display_buf img.width, img.height, buf, gamma
      puts
      display_diff img.width, img.height, diff_buf, gamma
      puts progress_bar Math::log(max_diff / max, Float::EPSILON), max_diff.to_s
    end
  end
rescue Interrupt
ensure
  puts "\e[?25h\e[?1049l"
  display_buf img.width, img.height, buf, gamma
end
