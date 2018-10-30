require 'io/console'

def right_multiline(width, left, rights)
  left_width = left.size
  lines = [left]
  rights.each do |right|
    lines << " " * left_width if lines.last.size + right.size + 1 >= width
    lines[-1] += " " + right
  end
  lines.join "\n"
end

alphabet = [*"A" .. "Z"]
edges = [gets.chomp, gets.chomp]
        .map{|line| line.chars.map{|c|alphabet.index c}}
        .transpose
out = []

keymap = "." + "-" * (edges.count - 1)
collisions = []

loop.with_index do |_, i|
  out << right_multiline(IO.console.winsize[1],
                         "%2d %s /" % [i, keymap],
                         collisions.map{|c|c.join("-")})
  break if /^\.*$/ === keymap
  
  prev_keymap = keymap
  prev_collisions = collisions
  
  key_pairs = edges.map{|es| es.map{|e| keymap[e]}}
  
  #assign keys to pairs of previous keys
  pair_keys = Hash.new{|h,k| h[k] = ""}
  keymap = key_pairs.zip(alphabet).map{|pair, a|
    case 
    when pair == ["-", "-"] 
      "-"
    else
      pair_keys[pair] += a; a
    end
  }.join
  
  #determine which pairs of new keys collide
  collisions = pair_keys.keys.combination(2).map{|pair_pair|
    pair_pair.map(&pair_keys) if pair_pair.transpose.all?{|nk1, nk2|
      nk1 == "-" || nk2 == "-" || prev_collisions.any?{|pc1, pc2|
        (pc1.include?(nk1) && pc2.include?(nk2)) || (pc1.include?(nk2) && pc2.include?(nk1))
      }
    }
  }.compact
  
  # combine keys with the same collision sets
  # pair_keys.values.combination(2).each do |nk1, nk2|
  #   k1c = collisions.select{|c| c.include? nk1}.map{|c| c - [nk1]}
  #   k2c = collisions.select{|c| c.include? nk2}.map{|c| c - [nk2]}
  #   next unless k1c == k2c
  #   keymap.tr!(nk2, nk1)
  #   collisions.reject! {|c| c.include? nk2}
  # end
  
  pair_keys.values.each do |nk|
    keymap.tr!(nk, ".") unless collisions.any?{|c| c.include? nk}
  end
  
  # compress the list of collisions
  loop do
    c1, c2 = collisions.combination(2).find {|c1, c2| (c1 & c2).one?} 
    break unless c1
    collisions.delete c1
    collisions.delete c2
    new_keys = (c1 - c2)[0] + (c2 - c1)[0]
    collisions.push [(c1 & c2)[0], new_keys.chars.sort.join].sort
  end
  
  collisions.sort!
end

puts "", out