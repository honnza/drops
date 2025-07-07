require "io/console"
require "chunky_png"

def display w, h, buf
  (0 ... (h + 1)/2).each do |hri|
    (0 ... w).each do |ci|
      top_r, top_g, top_b = buf[3 * (w * 2 * hri + ci), 3]
      bot_r, bot_g, bot_b = if hri * 2 + 1 == h
                              [0, 0, 0]
                            else 
                              buf[3 * (w * (2 * hri + 1) + ci), 3]
                            end
      print "\e[38;2;%d;%d;%d;48;2;%d;%d;%dm\u2580" % [top_r, top_g, top_b, bot_r, bot_g, bot_b]
    end
    puts "\e[0m"
  end
end

wrapping = ARGV.include? "-w"
ARGV.delete "-w"

if ARGV.length != 2 || !%w{normal linear}.include?(ARGV[0])
  puts "usage: ruby image-relax [-w] [filename] [method]"
  exit
end

img = ChunkyPNG::Canvas.from_file ARGV[1]
mask = img.pixels.map{_1 & 255 > 127}
buf = img.pixels.flat_map do
  if _1 & 255 > 127
    [_1 >> 24 & 255, _1 >> 16 & 255, _1 >> 8 & 255].map &:to_f
  else
    [rand * 255, rand * 255, rand * 255]
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
    buf.each_index do
      unless mask[_1 / 3]
        buf[_1] += diff_buf[_1] / 8
        buf[_1] = buf[_1].clamp(0.0, 255.0)
      end
    end
    IO.console.cursor = [0, 0]
    display img.width, img.height, buf if t % 16 == 0
  end
rescue Interrupt
ensure
  puts "\e[?25h\e[?1049l"
  display img.width, img.height, buf
end
