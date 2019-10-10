def gc x; (255 * x**(1/2.2)).round; end

def find_collatz(x, triple_even = true)
  paths_to = {x => [""]}
  search_queue = [x]
  search_queue.each do |n|
    return paths_to[n].map do |path|
      path.split("*").map{|part| part.length.to_s(16)}.join.gsub(/00(?=(00)*[^0])/, ";")
    end if n == 1
    [(n/2 if n.even?), (3*n+1 if n.odd? || triple_even)].compact.each do |nn|
      npaths = paths_to[n].map{|npath| npath + (nn > n ? "*" : "/")}
      if paths_to[nn]
        paths_to[nn].concat npaths if (paths_to[nn][0].length == npaths[0].length)
      else
        paths_to[nn] = npaths
        search_queue << nn
      end
    end
  end
  return nil
end

def combinations_3(x, y, z, n)
  0.upto(n) do |n_x|
    0.upto(n-n_x) do |n_y|
      n_z = n - n_x - n_y
      c = 1
      [[x, n_x], [y, n_y], [z, n_z]].each do |(d, n_d)| 
        c *= d.downto(d - n_d + 1).reduce(1, :*) / 1.upto(n_d).reduce(1, :*)
      end
      print "#{c} "
    end
    puts
  end
end

def ext_euclid(m, n)
  q = [nil, nil]
  r = [m,n]
  c_n = [1, 0]
  c_m = [0, 1]
  loop.with_index(2) do |_, i|
    q[i] = r[i-2] / r[i-1]
    [r, c_m, c_n].each{|col| col[i] = col[i-2] - col[i-1] * q[i]}
    return [q, r, c_m, c_n].transpose if r[i] == 0
  end
end

def snake_boxing(w,h)
  snakes = [*0...h].product([*0...w]).map{|pt| [pt]}
  free_ends = snakes.map{|s| [s[0], s]} * 2
  loop do
    pair = free_ends.combination(2).to_a.shuffle.find{|((x1, y1), s1), ((x2, y2), s2)|
      # s1.product(s2).count{|((x1, y1), (x2, y2))| (x1-x2).abs + (y1-y2).abs == 1} == 1 &&
      s1 != s2 && (x1-x2).abs + (y1-y2).abs == 1
    }
    return snakes if pair.nil?
    
    pair[0][1].reverse! unless pair[0][1].last == pair[0][0]
    pair[1][1].reverse! unless pair[1][1].first == pair[1][0]
    new_snake = pair[0][1] + pair[1][1]
    snakes << new_snake

    pair.each do |es|
      snakes.delete(es[1])
      free_ends.delete_at(free_ends.find_index(es))
      free_ends.find{|(e2, s2)| s2 == es[1]}[1] = new_snake
    end
  end
end

def generate_hypergrid(total, &op)
  r = [rand(total)]
  until r.size >= total
    gen = rand(total)
    r |= r.map{|x| op.(x, gen)}
    p [gen, r.size]
  end
  r
end

def generate_group(first, gens, &op)
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

def digitwise_sum(b)
  f = Proc.new{|x, y| (x + y) % b + (x < b && y < b ? 0 : f[x/b, y/b] * b)}
end

x = generate_group(rand(144), x=[*0..143].shuffle, &digitwise_sum(12))

#x=[*0..99].shuffle; x.each_slice(20).map{|x| puts x.join " "}
puts x.map{|x|(x/12).to_s(12)}.join; puts x.map{|x|(x%12).to_s(12)}.join
#x.map{|e|puts e;g=gets; x<< g+e.to_s if g[/./]}

h="";loop{h+=rand(2).to_s;print h;h="" if gets[/./]}

def inversion_count_histogram(n)
  r = Hash.new {|h, k| h[k] = 0}
  [*1 .. n].permutation{|x| r[x.combination(2).count{|c| c[0] > c[1]}] += 1}
  p r.values.join(", ")
  p r.values.sum

end

#algorithm taken from https://oeis.org/A008302
def inversion_count_table(n)
  res = Hash.new do |h, (n, k)|
    # puts "evaluating [%d, %d]" % [n, k]
    h[[n, k]] = if n == 1
                  (k == 0) ? 1 : 0
                elsif k < 0
                  0
                else
                  res[[n, k-1]] + res[[n-1, k]] - res[[n-1, k-n]]
                end
    # puts "res[[%d, %d]] = %d" % [n, k, res[[n, k]]]
    res[[n, k]]
  end
  
  (0..).lazy.map{|k| res[[n, k]]}.take_while{|r| r > 0}.force
end
