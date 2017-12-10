require 'json'
require 'zlib'
require 'set'

@elems = []
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
    @children = parent_pairs.reduce([], :|)
    recalc
  end

  def tries_adj; @tries - $elems_done; end # genuine tries = all tries - skips

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

    parents = @parent_pairs.reduce([], &:|)
    ancestors = parents.reduce(parents){|a, e| a | e.ancestors}
    if ancestors != @ancestors
      @ancestors = ancestors
      @children.each(&:recalc)
    end
  end

  def category
    /(.*?) ?-/ === @name ? $1 : :"no category"
  end
end

class Pair < Array
  def initialize e1, e2
    self[0] = e1
    self[1] = e2

  end

  def priority
    [
      $eagerSkip && self.any?{|el| el.done} ? 1 : 0,
      self.map(&:priority).reduce(:+),
      self.map{|e|-e.complexity}.reduce(:+)
    ]
  end
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

def add elem_name, parent_names
  parents = parent_names.map do |p_name|
    parent = @elems.find{|e| e.name == p_name}
    (puts "unknown element #{p_name}"; return nil) unless parent
    parent
  end
  elem = @elems.find{|e| e.name == elem_name}
  if elem
    elem.add_pair parents
  else
    elem = Elem.new elem_name, [parents]
    @elems << elem
    @elems_by_last_seen << elem
    @elems.each{|e2| @pairs << Pair.new(e2, elem) unless e2.name.end_with? '*'} unless elem_name.end_with? '*'
  end
  $cur_element = nil
  $cur_category = elem.category
  parents.each{|p|p.update_text; p.successes += 1; p.children |= [elem]}
  elem.update_text
  @categories |= [elem.category]
  parents + [elem]
end

def pop pair = nil
  if pair
    pair = @pairs.find{|p| p.sort_by(&:name) == pair.sort_by(&:name)}
  else
    pair = @pairs.max_by{|p| [p.priority, rand]}
  end
  return unless pair
  pair.each{|e| e.update_text; e.tries += 1}
  @pairs.delete pair
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
  $cur_element = out_pair.include?($cur_element) ? $cur_element : out_pair.first
  $cur_category = out_pair.last.category
  $last_pair = pair
  @elems_by_last_seen.sort_by!.with_index{|e, i| [pair.include?(e) ? 0 : 1, i]}
  out_pair.join(" + ")
end

def rename from, to
  elem = @elems.find{|e|e.name==from}
  raise ArgumentError, "element #{from} doesn't exist" unless elem
  raise ArgumentError, "element #{from} already exists" if @elems.any?{|e| e.name == to}
  old_cat = elem.category
  elem.name = to
  @categories |= [elem.category]
  @categories.delete old_cat unless @elems.any?{|e| e.category == old_cat}
end

def reset_pairs
  @pairs = (@elems.combination(2).to_a + @elems.map{|e| [e, e]}).map do |e1, e2|
    Pair.new(e1, e2) unless e1.name.end_with?("*") || e2.name.end_with?("*")
  end.compact
  @elems.each{|e| e.tries = 0}
  @elems.each{|e| e.parent_pairs.each{|pp| pop pp}}
end

def p_stats
  puts "elements by pairs left"
  @elems.map{|e|@elems.size - e.tries + 1}.group_by{|x|x}.to_a.sort.reverse.map{|a| p [a[0], a[1].size]}
  puts "elements by outdegree"
  groups = @elems.map(&:successes).group_by{|x| x}
  (0..groups.keys.max).map{|k| p [k, groups.fetch(k, []).length]}
  pairs_expected = @elems.length * (@elems.length + 1) / 2
  puts "pairs popped: %f%%" % ((1 - @pairs.length.fdiv(pairs_expected)) * 100)
  json = JSON.generate serialize_data
  puts "data compression ratio: " + (Zlib::BEST_SPEED .. Zlib::BEST_COMPRESSION).map{
    |q| "%.2f%%" % (Zlib.deflate(json, q).length.to_f / json.length * 100)
  }.join(", ")
end

def show_elems key, elem_name = nil
  sort_orders = {
    name: -> _, _ {""},
    history: -> _, i {i + 1},
    ancestors: -> e, _ {e.ancestors.count},
    outdegree: -> e, _ {e.successes},
    complexity: -> e, _ {e.complexity},
    importance: -> e, _ {@elems.select{|f| f.ancestors.include? e}.count}
  }

  elem = @elems.find{|e| e.name == elem_name}
  raise ArgumentError if elem_name && elem.nil?
  @elems.map.with_index{|e, i| [sort_orders[key.to_sym][e, i], e]}
        .sort_by{|ix, e| [ix, e.name]}
        .each do |ix, e|
    if !elem || elem == e || elem.ancestors.include?(e)
        e.parent_pairs.each do |pair|
          if pair.empty?
            puts "@#{ix} add #{e.name}"
          else
          puts "@#{ix} #{pair.map(&:name).join(" + ")} = #{e.name}"
        end
      end
    end
  end
  puts
end

BASE64_ALPHABET = [*?A..?Z, *?a..?z, *?0..?9, ?+, ?/]
def elem_pair_iterator
  Enumerator.new do |y|
    @elems.each_with_index do |e2, i2|
      @elems[0 .. i2].each do |e1|
        y.yield [e1, e2]
      end
    end
  end
end

def serialize_data
  pair_set = Set.new @pairs
  {
    e: @elems.map{|e| [e.name, e.parent_pairs.map{|pair|pair.map{|e|@elems.index e}}, e.done ? 1 : 0]},
    p: elem_pair_iterator.map{|pair| pair_set.include?(pair) ? ?1 : ?0}
                .each_slice(6).map{|bits|
                  bits << 0 until bits.length == 6
                  BASE64_ALPHABET[bits.join.to_i(2)]
                }.join
  }
end

def deserialize_data data
  @elems = data[:e].map{|e| el = Elem.new(e[0], []); el.done = e[2] == 1; el}
  data[:e].each do |e|
    elem = @elems.find{|elem| elem.name == e[0]}
    e[1].each do |ixes|
      pair = ixes.map{|ix| @elems[ix]}
      elem.add_pair pair
      pair.each{|p|p.update_text; p.successes += 1; p.children |= [elem]}
    end
  end
  bits = data[:p].chars.flat_map{|char| ("%06b" % BASE64_ALPHABET.index(char)).chars}
  @pairs = elem_pair_iterator.zip(bits)
                    .map{|pair, bit|
                      if bit == ?1
                        Pair.new(*pair)
                      else
                        pair.each{|e| e.tries += 1}
                        nil
                      end
                    }.compact
  @categories = @elems.map(&:category).uniq
  @elems_by_last_seen = @elems.sort_by{|el| [-el.priority, rand]}
  $elems_done = @elems.count &:done
end

puts "Welcome to Alchemy.rb"
if ARGV.include? "--eager-skip"
  $eagerSkip = true
  puts "will be skipping eagerly"
end
loop do
  case $stdin.gets
    when /^(.*?)\s*\+\s*(.*?)\s*\=\s*(.*?)$/ # p1+p2=p3 with optional spaces
      r = add($3, [$1, $2]); puts "ok; %s + %s = %s" % r.map(&:to_s) if r
    when /^add (.*)$/ then puts "ok; %s" % add($1, []).map(&:to_s)
    when /^\=\s*(.*?)$/
      r = add($1, $last_pair.map(&:name)); puts "ok; %s + %s = %s" % r.map(&:to_s) if r
    when /^(pop)?$/ then loop { puts pop; break if $last_pair.all?{|e| !e.done}}
    when /^done$/ then puts pop until @pairs.empty?
    when /^s\/(.*?)\/(.*?)\// then (rename $1, $2; puts "ok") rescue puts $!
    when /^stats$/ then p_stats
    when /^show elements by (.+)$/ then show_elems $1 #rescue puts "?"
    when /^show history$/ then show_elems "history"
    when /^next chapter$/ then (reset_pairs; puts "ok") rescue puts $!
    when /^skip (.*)$/
      e = @elems.find{|e| e.name == $1}
      if e then e.done = true; puts "ok" else puts "unknown element #{$1}" end
    when /^show history of (.*)$/
      show_elems "history", $1 rescue puts "unknown element #{$1}"
    when /^save as (.*)$/
      json = JSON.generate(serialize_data)
      Zlib::GzipWriter.open($1 + ".alchz", Zlib::BEST_COMPRESSION) {|gz| gz.write json}
      zip_size = File.size $1 + ".alchz"
      pct_string = "%.2f" % (zip_size.to_f / json.size * 100)
      puts "ok; #{zip_size} bytes written (#{pct_string}% compression ratio)"
    when /^load from (.*)$/
      Zlib::GzipReader.open($1 + ".alchz") do |gz|
        json = JSON.parse gz.read, symbolize_names: true
        deserialize_data json
        puts "\e[Gok; #{@elems.length} elements"
      end
    when /^(q|quit|exit)$/ then exit
    else puts '?'
  end
end
