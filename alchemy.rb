require 'json'
require 'zlib'
require 'set'

$elems = []
@pairs = []
@categories = []
@prev_pop = nil
$cur_category = nil
$cur_element = nil
$last_pair = nil
@elems_by_last_seen = []
$elems_done = 0

class String
  def ansi_color fg, bg = nil
    args = [
      (30 + fg % 8 if fg),
      (40 + bg if bg),
      (1 if fg && fg >= 8)
    ].compact
    "\e[#{args.join ?;}m#{self}\e[0m"
  end
end


class Elem
  attr_reader :ancestors, :complexity, :done
  attr_accessor :name, :tries, :successes, :children, :parent_pairs
  def initialize name, parent_pairs
    @done = false
    @name = name
    @tries = 0
    @successes = 0
    @parent_pairs = parent_pairs
    @children = []
    @distances = [[]]
    recalc
  end

  def tries_adj; @tries - $elems_done; end # genuine tries = all tries - skips
  #def tries_adj; @tries; end

  def priority
    Rational(@successes + 1, tries_adj + 2) * (self == $cur_element || category == $cur_category ? 2 : 1)
  rescue ZeroDivisionError
    Float::INFINITY # this can happen if the element is yet to be skipped twice
  end
  
  def done= val
    $elems_done += 1 if val
    $elems_done -= 1 if @done
    @done = val
  end

  def update_text
    @text = "%s(%s %s | %d/%d => %.1f%%)" % [@name, @complexity, @ancestors.count, @successes, tries_adj, 100*priority]
    @text = case
      when done then @text.ansi_color(1)
      when self == $cur_element then @text.ansi_color(8)
      when category == $cur_category then @text.ansi_color(15)
      else @text.ansi_color(11)
    end
  end

  def to_s
    @text
  end

  def add_pair parents
    @parent_pairs << parents
    recalc
  end

  def recalc
    complexity = @parent_pairs.map do |pair|
      if pair.empty?
        1
      else
        pair.inject(0){|a,e|a + (e.complexity || Float::INFINITY)}
      end
    end.min
    if @complexity != complexity
      puts "#{@name} complexity decreased to #{complexity}" if @complexity && complexity < @complexity
      @complexity = complexity
      @children.each(&:recalc)
    end

    parents = @parent_pairs.flatten.uniq
    ancestors = parents.reduce(parents){|a, e| a | e.ancestors}
    if ancestors != @ancestors
      @ancestors = ancestors
      @children.each(&:recalc)
    end
  end

  def category
    /(.*?) ?-/ === @name ? $1 : :"no category"
  end
  
  def recalc_distances
    neighbors = lambda do |node|
      if node == :root
        $elems.select{|e| e.parent_pairs.any? &:empty?}
      else
        node.parent_pairs.flatten.uniq | node.children | [(:root if node.parent_pairs.any? &:empty?)].compact
      end
    end
    
    @distances = [[self]]
    loop.with_index(1) do |depth, ix|
      @distances << @distances.reduce(@distances.last.map(&neighbors).flatten.uniq){|a, e| a - e}
      break if @distances.last.empty?
    end
  end
  
  def distance_to other; @distances.find_index{|ds| ds.include? other}; end
end

class Pair < Array
  def initialize e1, e2
    self[0] = e1
    self[1] = e2
  end

  def priority
    [
      $eager_skip && self.any?{|el| el.done} ? 1 : 0,
      self.map(&:priority).reduce(:+) + 1.0 / self.reduce(&:distance_to),
      self.map{|e|-e.complexity}.reduce(:+)
    ]
  end
  
  # hashes mostly act as numbers, but sometimes as 32-bit numbers: https://bugs.ruby-lang.org/issues/14218
  def hash; map{|el| "#{el.name} @ #{el.hash}"}; map(&:hash).reduce(:+) % 2**32; end
  def ==(other); (self[0] == other[0] && self[1] == other[1]) || (self[0] == other[1] && self[1] == other[0]); end
  alias eql? ==
end

 ##############################################################################

=begin

class FreeGroupoid
  attr_reader :vLeft, :vRight, :value

  def initialize vLeft, vRight, value
    @vLeft  = vLeft
    @vRight = vRight
    @value  = value
  end

  One = FreeGroupoid.new nil, nil, 0
  @@byValue = [One]
  @@byParents = Hash.new {|hash, key1| hash[key1] = Hash.new { |hash, key2| hash[key2] = FreeGroupoid.new key1, key2, nil }}

  def + other
    self.realize
    other.realize
    a1, a2 = [self, other].sort
    @@byParents[a1.value][a2.value]
  end

  def realize
    unless @value
      @value = @@byValue.length
      @@byValue[@value] = self
    end
  end

  def <=> other
    return self.value <=> other.value if self.value && other.value
    return -1 if  self.value
    return  1 if other.value
    [self.vLeft, self.vRight] <=> [other.vLeft, other.vRight]
  end

  def to_s
    return "a0" if @value == 0
    (@value ? "a#{@value} = " : "") + "(a#{@vLeft}+a#{@vRight})"
  end

  protected :vLeft, :vRight, :value, :realize
end

=end

 ##############################################################################

 class BitPacker
   def self.period str; (1..str.length).find{|i| str[i .. -1] == str[0 ... -i]}; end

   ALPHABET_B64 = [*?A..?Z, *?a..?z, *?0..?9, ?+, ?/]
   ALPHABET6  = [*?0..?9, *?a..?z, *?A..?Z, ?[, ?]]
   ALPHABET13 = (0x20 .. 0x7e).map(&:chr) - ["\\", "\""]
   PAD_HASHES = Hash[[1, 2, 3, 4, 5, 6, 13].map do |word_width|
     [word_width, Hash.new do |h, k| 
       buf = k.dup
       until buf.length == word_width
         buf.push(period(buf + [true]) < period(buf + [false]))
       end
       puts "%s => %s" % [k, buf].map{|a| a.map{|c| c ? 1 : 0}.join}
       h[k] = buf
     end]
   end]
   
   def initialize word_width, base64 = false, str = ""
     @word_width = word_width
     @bit_buffer = []
     @char_buffer = str.reverse.chars
     @base64 = base64
   end
   def skip_pad; @bit_buffer = []; end
   def shift_bit
     @bit_buffer = chars_to_bits @char_buffer.pop(@word_width == 13 ? 2 : 1) if @bit_buffer.empty?
     @bit_buffer.shift
   end
   
   def push_bit bit
     @bit_buffer << bit
     if @bit_buffer.length == @word_width
       @char_buffer.concat bits_to_chars @bit_buffer
       @bit_buffer = []
     end
   end
   
   def pad
     unless @bit_buffer.empty?
       @char_buffer.concat bits_to_chars PAD_HASHES[@word_width][@bit_buffer]
       @bit_buffer = []
     end
   end

   def result; pad; @char_buffer.join; end
 
   def chars_to_bits chars
     chars << "0" until chars.length == (@word_width == 13 ? 2 : 1)
     if @word_width == 13
       bit_int = chars.map{|c| ALPHABET13.find_index c}.reduce{|a, b| a * ALPHABET13.length + b}
     else
       bit_int = (@base64 ? ALPHABET_B64 : ALPHABET6).find_index chars[0]
     end
     
     (@word_width-1).downto(0).map{|i| bit_int[i] == 1}
   end
   
   def bits_to_chars bit_buffer
     bit_int = bit_buffer.map(&{false => 0, true => 1}).reduce{|a, b| 2 * a + b}
     if @word_width == 13
       high, low = bit_int.divmod ALPHABET13.length
       [ALPHABET13[high], ALPHABET13[low]]
     else
       [(@base64 ? ALPHABET_B64 : ALPHABET6)[bit_int]]
     end
   rescue; p self; raise
   end  
 end

###############################################################################

def add elem_name, parent_names
  parents = parent_names.map do |p_name|
    parent = $elems.find{|e| e.name == p_name}
    (puts "unknown element #{p_name}"; return nil) unless parent
    parent
  end
  elem = $elems.find{|e| e.name == elem_name}
  if elem
    elem.add_pair parents
  else
    elem = Elem.new elem_name, [parents]
    $elems << elem
    @elems_by_last_seen << elem
    $elems.each{|e2| @pairs << Pair.new(e2, elem) unless e2.name.end_with? '*'} unless elem_name.end_with? '*'
  end
  $cur_element = nil
  $cur_category = elem.category
  parents.each{|p|p.update_text; p.successes += 1; p.children |= [elem]}
  elem.update_text
  @categories |= [elem.category]
  $elems.each &:recalc_distances
  parents + [elem]
end

def pop names = nil, only_skip: false
  loop do
    if names
      pair = @pairs.find{|p| p.sort_by(&:name) == names.sort_by(&:name)}
    else
      pair = @pairs.max_by{|p| [p.priority, rand]}
    end
    return unless pair
    out_pair = pair.sort_by{|e| [
      e == $cur_element ? 0 : 1,
      e.category == $cur_category ? 0 : 1,
      - @categories.index(e.category),
      @elems_by_last_seen.index(e)
    ]}
    top_cats = ((@prev_pop || []) | pair).group_by{|e| e.category}
                                         .map{|k,v| k if v.size >= 2}.compact
    old_cats = @categories.dup
    @categories.sort_by!.with_index{|cat, i| [(top_cats.include? cat) ? 0 : 1, i]}
    @prev_pop = pair

    break if !pair.any?(&:done) && only_skip

    pair.each{|e| e.update_text; e.tries += 1}
    puts out_pair.join(" + ") + " @ " + pair.reduce(&:distance_to).to_s
    @pairs.delete pair
    redo if pair.any? &:done
    
    $cur_element = out_pair.include?($cur_element) ? $cur_element : out_pair.first
    $cur_category = out_pair.last.category
    $last_pair = pair
    @elems_by_last_seen.sort_by!.with_index{|e, i| [pair.include?(e) ? 0 : 1, i]}
    break
  end
end

def rename from, to
  elem = $elems.find{|e|e.name==from}
  raise ArgumentError, "element #{from} doesn't exist" unless elem
  raise ArgumentError, "element #{from} already exists" if $elems.any?{|e| e.name == to}
  old_cat = elem.category
  elem.name = to
  @categories |= [elem.category]
  @categories.delete old_cat unless $elems.any?{|e| e.category == old_cat}
end

def reset_pairs
  @pairs = ($elems.combination(2).to_a + $elems.map{|e| [e, e]}).map do |e1, e2|
    Pair.new(e1, e2) unless e1.name.end_with?("*") || e2.name.end_with?("*")
  end.compact
  $elems.each{|e| e.tries = 0}
  $elems.each{|e| e.parent_pairs.each{|pp| pop pp}}
end

def p_stats
  puts "elements by pairs left"
  $elems.map{|e|$elems.size - e.tries + 1}.group_by{|x| x}.to_a.sort.reverse.each{|a| p [a[0], a[1].size]}
  puts "elements by outdegree"
  groups = $elems.map(&:successes).group_by{|x| x}
  (0..groups.keys.max).each{|k| p [k, groups.fetch(k, []).length]}
  puts "pairs by distance"
  @pairs.map{|pair| pair.reduce(&:distance_to)}.sort.group_by{|x| x}.to_a.sort.each{|a| p [a[0], a[1].size]}
end

def show_elems key, elem_name = nil, topo_sort = true
  sort_orders = {
    name: -> _, _ {""},
    history: -> _, i {i + 1},
    ancestors: -> e, _ {e.ancestors.count},
    outdegree: -> e, _ {e.successes},
    complexity: -> e, _ {e.complexity},
    importance: -> e, _ {$elems.select{|f| f.ancestors.include? e}.count}
  }

  elem = $elems.find{|e| e.name == elem_name}
  raise ArgumentError if elem_name && elem.nil?
  triples = $elems.map.with_index{|e, i| [sort_orders[key.to_sym][e, i], e]}.sort_by{|ix, e| [ix, e.name]}
                  .map{|ix, e| e.parent_pairs.map{|pp| [ix, e, pp] if !elem || elem == e || elem.ancestors.include?(e)}}
                  .flatten(1).compact
  
  if topo_sort
    triples_sorted = []
    insert = lambda do |(ix1, e1, pp1)|
      blocker = triples.find{|ix2, e2, pp2| !triples_sorted.any?{|ix3, e3, pp3| e2 == e3} && pp1.any?{|par| par == e2}}
      if blocker
        insert[blocker]
        redo
      else
        triples_sorted << [ix1, e1, pp1] unless triples_sorted.include? [ix1, e1, pp1]
      end
    end
    triples.map &insert
    triples = triples_sorted
  end
  
  triples.each do |ix, e, pp|
    puts "@#{ix} #{pp.empty? ? "add" : "#{pp.join " + "} ="} #{e.name}"
  end
end

def serialize_data word_width = 6, pad = false
  pair_set = Set.new @pairs.map
  bit_packer = BitPacker.new word_width
  $elems.each_with_index do |e1, i1|
    $elems[0 .. i1].each{|e2| bit_packer.push_bit(!!pair_set.delete?(Pair.new e2, e1))}
    bit_packer.pad if pad
  end
  raise "bug: pairs #{pair_set.map{|pair|pair.map(&:name)}} unaccounted for" unless pair_set.empty?
  {
    e: $elems.map{|e| [e.name, e.parent_pairs.map{|pair|pair.map{|e|$elems.index e}}, e.done ? 1 : 0]},
    pt: word_width.to_s + (pad ? "p" : ""),
    p: bit_packer.result[/^(.*[^0])*(?=0*$)/]
  }
end

def deserialize_data data
  (data[:pt] || "6") =~ /^(\d|13)(p?)$/; word_width = $1.to_i; pad = $2 == "p"
  $elems = data[:e].map{|e| el = Elem.new(e[0], []); el.done = e[2] == 1; el}
  data[:e].each do |e|
    elem = $elems.find{|elem| elem.name == e[0]}
    e[1].each do |ixes|
      pair = ixes.map{|ix| $elems[ix]}
      elem.add_pair pair
      pair.each{|p|p.update_text; p.successes += 1; p.children |= [elem]}
    end
  end
  bit_packer = BitPacker.new word_width, data[:pt].nil?, data[:p]
  @pairs = []
  $elems.each_with_index do |e1, i1|
    $elems[0 .. i1].each do |e2|
      if bit_packer.shift_bit then @pairs << Pair.new(e2, e1) else e1.tries +=1 ; e2.tries += 1; end
    end
    bit_packer.skip_pad if pad
  end
  @categories = $elems.map(&:category).uniq
  $elems.each &:recalc_distances
  @elems_by_last_seen = $elems.sort_by{|el| [-el.priority, rand]}
  $elems_done = $elems.count &:done
end

def mk_save
  best_str = nil
  [false, true].each do |pad|
    [1, 2, 3, 4, 5, 6, 13].each do |word_width|
      json = JSON.generate serialize_data(word_width, pad)
      # StringIO defaults to "" rather than String.new
      # which differ in that String.new.encoding is ASCII-8BIT
      # but "".encoding is UTF-8, causing us to misestimate the byte length
      io = StringIO.new String.new 
      gz = Zlib::GzipWriter.new(io, Zlib::BEST_COMPRESSION)
      gz.write json
      gz.close
      puts "#{word_width}#{pad ? "p" : "c"} #{io.string.length}"
      best_str = io.string if !best_str || best_str.length > io.string.length
    end
  end
  best_str
end

def mk_save_opt
  pair = nil
  $elems.sort_by!.with_index{|el, ix| [el.tries, ix]}
  p $elems.map{|el| "#{el.name}"}
  mk_save
end

###############################################################################

puts "Welcome to Alchemy.rb"
if ARGV.include? "--eager-skip"
  $eager_skip = true
  puts "will be skipping eagerly"
end
loop do
  case $stdin.gets
    when /^(.*?)\s*\+\s*(.*?)\s*\=\s*(.*?)$/ # p1+p2=p3 with optional spaces
      r = add($3, [$1, $2]); puts "ok; %s + %s = %s" % r.map(&:to_s) if r
    when /^add (.*)$/ then puts "ok; %s" % add($1, []).map(&:to_s)
    when /^\=\s*(.*?)$/
      r = add($1, $last_pair.map(&:name))
      puts "ok; %s + %s = %s" % r.map(&:to_s) if r
      pop(only_skip: true) if $eager_skip
    when /^(pop)?$/ then pop
    when /^done$/ then puts pop until @pairs.empty?
    when /^s\/(.*?)\/(.*?)\// then (rename $1, $2; puts "ok") rescue puts $!
    when /^stats$/ then p_stats
    when /^show elements by (.+)$/ then show_elems $1 #rescue puts "?"
    when /^show history$/ then show_elems "history"
    when /^next chapter$/ then (reset_pairs; puts "ok") rescue puts $!
    when /^skip (.*)$/
      e = $elems.find{|e| e.name == $1}
      if e then e.done = true; puts "ok" else puts "unknown element #{$1}" end
      pop(only_skip: true) if $eager_skip
    when /^show history of (.*)$/
      show_elems "history", $1 rescue puts "unknown element #{$1}"
    when /^save as (.*)$/
      save = mk_save_opt
      File.open($1 + ".alchz", "wb"){|f| f.write save}
      puts "\e[Gok; #{$elems.length} elements, #{@pairs.length} pairs"
      puts "wrote #{save.length} bytes to disk"
    when /^load from (.*)$/
      Zlib::GzipReader.open($1 + ".alchz") do |gz|
        json = JSON.parse gz.read, symbolize_names: true
        deserialize_data json
        puts "\e[Gok; #{$elems.length} elements, #{@pairs.length} pairs"
      end
    when /^(q|quit|exit)$/ then exit
    else puts '?'
  end
end
