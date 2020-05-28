require 'io/console'
include Math
wh, ww = IO.console.winsize

func = ->x, c {x**2 + c}
labs = ->c {log(c.abs)}
plte = ->c {c.finite? ? [sin(c), sin(c + PI*2/3), sin(c - PI*2/3)].map{|c| 127*c + 127} : [0, 0, 0]}

miny = -1.0
maxy = 1.0
minx = -0.5 * ww/wh
maxx = 0.5 * ww/wh

stepy = (maxy - miny) / (wh - 0.5)
stepx = (maxx - minx) / (ww - 1)

iter_count = lambda do |c, func|
  x = 0
  1000.times do |i|
    x = func[x, c]
    return i if x.abs > 2
  end
  return 1/0.0
end

wh.times do |im|
  ww.times do |re|
    c = Complex(minx + re * stepx, miny + im * stepy)
    fore = plte[iter_count[c, func]]
    c = Complex(minx + re * stepx, miny + (im+0.5) * stepy)
    back = plte[iter_count[c, func]]
    print "\e[38;2;%d;%d;%d;48;2;%d;%d;%dm▀" % (fore + back)
  end
end

puts "\e[0m"
