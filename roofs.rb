def pad ary
  row = ary.first.map{-1}
  ([row] + ary + [row]).map{|r| [-1] + r + [-1]}
end

def parse str
  str.split("\n").map{|r| r.chars.map(&{" " => -1, "*" => Float::INFINITY})}
end

def squares ary, size
  ary.each_cons(size).map do |rows|
    rows.map{|row| row.each_cons(size).to_a}.transpose
  end
end

def consmap2d ary, size
  squares(ary, size).map{|sqrow| sqrow.map{|sq| yield sq}}
end

def relax ary
  loop do
    new = consmap2d(pad(ary), 3){|sq| sq[1][1] == -1 ? -1 : sq.flatten.min + 1}
    return new if new == ary
    ary = new
  end
end

def semidouble ary, op
  ary.zip(ary.each_cons(2).map{|r1,r2|r1.zip(r2).map(&op)}).flatten(1).compact.transpose
end

def heightmap str
  relax(semidouble(semidouble(semidouble(semidouble(pad(parse str),:max),:max),:min),:min))
end

def render heightmap
  puts consmap2d(heightmap, 3){|sq|
    next " " if sq[1][1] == -1
    hwall = sq[0][1] == -1 || sq[2][1] == -1
    vwall = sq[1][0] == -1 || sq[1][2] == -1
    next "+" if hwall && vwall
    next "-" if hwall
    next "|" if vwall
    next "+" if sq.flatten.min == -1

    nws = sq[0][1] == sq[1][0]
    nes = sq[0][1] == sq[1][2]
    sws = sq[2][1] == sq[1][0]
    ses = sq[2][1] == sq[1][2]

    next "X"  if nws && nes && sws && ses
    next "V"  if nws && nes
    next "^"  if sws && ses
    next ">"  if nws && sws
    next "<"  if nes && ses
    next "/"  if nes && sws
    next "\\" if nws && ses
    next " "  if sq[0][1] != sq[2][1] || sq[1][0] != sq[1][2]
    next "|"  if sq[0][1] == sq[1][1]
    next "-"  if sq[1][0] == sq[1][1]
    ??
  }.map(&:join)
end
