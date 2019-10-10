require 'io/console'
require 'json'
require 'zlib'
require 'set'

def word_wrap words, width = IO.console.winsize[1] - 1
  words = words.split(" ") if words.is_a? String
  words[1 .. -1].reduce [words[0]] do |acc, word|
    if acc.last.length + word.length >= width
      acc << word
    else
      acc.last.concat " " + word
    end
    acc
  end
end

def print_time_to str
  time_start = Time.now
  r = yield
  puts "%.2fs to %s" % [Time.now - time_start, str]
  r
end

$elems = []
@categories = []
@prev_pop = nil
$cur_category = nil
$cur_element = nil
$last_pair = nil
@elems_by_last_seen = []
$elems_done = 0

$stats_keys = %i{pairs_used cache_misses future_dropped future_sort_steps}
$stats = Hash[$stats_keys.map{|k| [k, 0]}]

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

class Array
  def lossy_inspect(max_len)
    return inspect if inspect.length <= max_len
    copy = dup
    skipped = 0
    (copy.pop; skipped += 1) while copy.inspect.length + skipped.to_s.length + 7 > max_len
    copy.inspect.sub "]", ", ...(#{skipped})]"
  end
  
  def rjust(n, elem = nil); [elem] * [n - size, 0].max + self; end
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

  def tries_adj; $eager_skip ? @tries - $elems_done : @tries; end # genuine tries = all tries - skips
  #def tries_adj; @tries; end

  def priority
    Rational(@successes + 1, tries_adj + 2) * ($use_cats && (self == $cur_element || category == $cur_category) ? 2 : 1)
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
      when self == $cur_element && $use_cats then @text.ansi_color(8)
      when category == $cur_category || !$use_cats then @text.ansi_color(15)
      else @text.ansi_color(11)
    end
  end

  def to_s; @text; end
  def inspect; "Elem<<#{name}>>"; end

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
      node.parent_pairs.flatten.uniq | node.children
    end
    
    @distances = [[self]]
    loop.with_index(1) do |depth, ix|
      @distances << @distances.reduce(@distances.last.map(&neighbors).flatten.uniq){|a, e| a - e}
      break if @distances.last.empty?
    end
    @directed_dists = [[self]]
    loop.with_index(1) do |depth, ix|
      @directed_dists << @directed_dists.reduce(@directed_dists.last.map(&:parent_pairs).flatten.uniq){|a, e| a - e}
      break if @directed_dists.last.empty?
    end
  end
  
  def distance_to other; @distances.find_index{|ds| ds.include? other}; end
  def directed_dist_to other; @directed_dists.find_index{|ds| ds.include? other}; end
  def bitonic_dist_to other; bitonic_path_to(other).length - 1; end
  
  def path_to other
    return [self] if self == other
    own_distance = distance_to(other)
    next_node = @distances[1].find{|node| node.distance_to(other) == own_distance - 1}
    [self] + next_node.path_to(other)
  end

  def directed_path_to other
    return [self] if self == other
    own_distance = directed_dist_to(other) or return nil
    next_node = @parent_pairs.flatten.uniq.find{|node| node.directed_dist_to(other) == own_distance - 1}
    [self] + next_node.directed_path_to(other)
  end
  
  def bitonic_path_to other
    ([self, *@ancestors] & [other, *other.ancestors]).map{|parent|
      left = self.directed_path_to(parent)
      right = other.directed_path_to(parent)
      left + right.reverse[1 .. -1]
    }.min_by(&:length)
  end

  ROOT = Elem.new("[]", [[]])
end

class Pair < Array
  def initialize e1, e2, via_parts: nil, elem_strs: nil
    self[0] = e1
    self[1] = e2
    @via_parts = via_parts
    @elem_strs = elem_strs
  end
  
  def bitonic_length
    @bitonic_length ||= reduce(&:bitonic_dist_to)
  end

  def priority
    [
      $eager_skip && self.any?(&:done) ? 1 : 0,
      self.map(&:successes).sort.reverse,
      self.map(&:tries).sort.reverse,
      self.map{|e|-e.complexity}.reduce(:+)
    ]
  end
  
  # hashes mostly act as numbers, but sometimes as 32-bit numbers: https://bugs.ruby-lang.org/issues/14218
  def hash; map{|el| "#{el.name} @ #{el.hash}"}; map(&:hash).reduce(:+) % 2**32; end
  def ==(other); (self[0] == other[0] && self[1] == other[1]) || (self[0] == other[1] && self[1] == other[0]); end
  alias eql? ==
  
  def update_text
    @via_parts = first.path_to(last)[1..-2].map(&:name) 
    @elem_strs = map &:to_s
  end
  
  def sort_by
    if (yield(first) <=> yield(last)) > 0
      Pair.new(last, first, via_parts: @via_parts.reverse, elem_strs: @elem_strs.reverse)
    else
      Pair.new(first, last, via_parts: @via_parts, elem_strs: @elem_strs)
    end
  rescue
    p [first, yield(first), last, yield(last)]
    raise
  end
  
  def to_s
    if @via_parts.empty?
      @elem_strs.join(" + ")
    else
      @elem_strs.join(" + ") + " via " + @via_parts.join(", ")
    end
  end
end

module Future
  include Enumerable
  class <<self
    def initialize
      @near_future = []
      @far_future = []
      self
    end
    
    private def ensure_near(n)
      while @near_future.size <= n
        pair = @far_future.max_by{|pa| [pa.priority, rand]}
        $stats[:future_sort_steps] += @far_future.size
        return if pair.nil?
        pair.each{|e| e.update_text; e.tries += 1}
        pair.update_text
        @near_future << pair
        @far_future.delete pair
        str = "peering into the future (@#{n} #{pair})"
        ww = IO.console.winsize[1]
        if n > 1
          str_len = str.gsub(/\e\[[^a-z]*[a-z]/i, "").length
          if str_len > ww
            puts str 
          else
            print str + " " * (ww - str_len)
          end
        end
      end
    end
    
    def rewind
      $stats[:future_dropped] += @near_future.size
      @near_future.each{|pa| pa.each{|e| e.tries -= 1}}
      @far_future.concat @near_future
      @near_future = []
      self
    end
    
    def pop; ensure_near 0; @near_future.shift; end
    def peek; ensure_near 0; @near_future.first; end
    def [](i); ensure_near i; @near_future[i]; end
    def concat(pairs); @far_future.concat pairs; end
    def <<(pair); @far_future << pair; end
    def unordered; @near_future + @far_future; end
    def length; @near_future.size + @far_future.size; end
    def empty?; length == 0; end
    
    def each
      if block_given?
        (0 ... length).each{|i| ensure_near i; yield @near_future[i]}
      else
        to_enum{length}
      end
    end
    
    def delete(pair); @near_future.delete(pair) || @far_future.delete(pair); end

    def elem_tries(elem)
      elem.tries - @near_future.flatten.count(elem)
    end
    def elem_done?(elem)
      elem.tries == $elems.count + 1 && !@near_future.any?{|pa| pa.include?(elem) && !pa.any?(&:done)}
    end
    
    def inspect
      @near_future.map.with_index{|pa, ix| "@#{ix} #{pa}\n"}.join + "... x #{@far_future.size}"
    end
  end
  
  
  initialize
end

class ElemElevatorEnumerator
  def initialize(state = nil); set_state(state); end
  
  def next
    case @prev_dir
    when :>
      r = $elems.select{|e|e.name > @prev_elem.name}.min_by(&:name)
      (@prev_dir = :<; r = $elems.max_by(&:name)) unless r
    when :<
      r = $elems.select{|e|e.name < @prev_elem.name}.max_by(&:name)
      (@prev_dir = :>; r = $elems.min_by(&:name)) unless r
    end
    @prev_elem = r
  end
  
  def peek
    s = get_state
    r = self.next
    set_state(s)
    r
  end
  
  def each
    if block_given?
      loop{yield self.next}
    else
      to_enum{$elems.count}
    end
  end
  
  def get_state
    ix = $elems.find_index(@prev_elem)
    ix && @prev_dir == :< ? ~ix : ix
  end
  
  def set_state(state)
    @prev_elem, @prev_dir =
        case
        when state.nil? then [Elem::ROOT,     :>]
        when state < 0  then [$elems[~state], :<]
        else                 [$elems[state],  :>]
        end
  end
end

$board_elevator = ElemElevatorEnumerator.new

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
   def zero_pad    
     unless @bit_buffer.empty?
       @bit_buffer << false until @bit_buffer.size == @word_width
       @char_buffer.concat bits_to_chars @bit_buffer
       @bit_buffer = []
     end
   end

   def result; pad; @char_buffer.join; end
   def result_trimmed; c = bits_to_chars []; result[/^.*?(?=#{c}*$)/]; end 
 
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
   rescue;; raise
   end  
 end

###############################################################################

def add elem_name, parent_names
  parents = parent_names.map do |p_name|
    parent = $elems.find{|e| e.name == p_name}
    (puts "unknown element #{p_name}"; return nil) unless parent
    parent
  end
  Future.rewind
  parents << Elem::ROOT if parents.count == 0
  elem = $elems.find{|e| e.name == elem_name}
  if elem
    elem.add_pair parents
  else
    elem = Elem.new elem_name, [parents]
    $elems << elem
    @elems_by_last_seen << elem
    Future.concat $elems.map{|e2| Pair.new(e2, elem)}
  end
  $cur_element = nil
  $cur_category = elem.category
  parents.each{|e| e.update_text; e.successes += 1; e.children |= [elem]}
  elem.update_text
  @categories |= [elem.category]
  [*$elems, Elem::ROOT].each(&:recalc_distances)
  if $elem_board
    parents.each{|parent| $elem_board.delete(parent) && $elem_board << nil}
    ix = $elem_board.find_index(nil)
    ($elem_board.delete_at(ix); $elem_board << elem) if ix
  end
  parents + [elem]
end

def rm_elem elem_name
  elem = $elems.find{|e| e.name == elem_name}
  Future.rewind
  return "unknown element #{elem_name}" unless elem
  return "can't remove #{elem_name}: has children" if elem.successes > 0
  $elems.each do |elem2|
    unless Future.delete(Pair.new(elem, elem2))
      elem.tries -= 1
      elem2.tries -= 1
    end
  end

  elem.parent_pairs.each do |pair|
    pair.each{|e| e.successes -= 1; e.children.delete(elem)}
  end
  $elems.delete(elem)
  $elem_board&.delete(elem) && $elem_board << nil
  @categories.delete elem.category unless $elems.any?{|e| e.category == elem.category}
  
  $elems.each(&:recalc)
  "done"
end

def pop(only_skip: false)
  loop do
    pair = Future.peek
    return unless pair
    out_pair = pair.sort_by{|e| [
      e == $cur_element ? 0 : 1,
      e.category == $cur_category ? 0 : 1,
      Future[1]&.include?(e) ? 0 : 1,
      - @categories.index(e.category),
      @elems_by_last_seen.index(e)
    ]}
    top_cats = ((@prev_pop || []) | pair).group_by{|e| e.category}
                                         .map{|k,v| k if v.size >= 2}.compact
    @categories.sort_by!.with_index{|cat, i| [(top_cats.include? cat) ? 0 : 1, i]}
    @prev_pop = pair

    break if !pair.any?(&:done) && only_skip
    Future.pop
    (puts out_pair.to_s; redo) if pair.any?(&:done)

    if $elem_board && !pair.all?{|el| $elem_board.include?(el)}
      new_board = $elem_board.dup
      pair.each do |el| 
        loop do
          ee = $board_elevator.peek
          new_board.delete ee
          new_board << ee unless ee.done 
          break if new_board.include? el
          $board_elevator.next
        end
      end
      $stats[:cache_misses] += (pair - $elem_board).size      
      while new_board.include?(nil) && new_board.size > $elem_board.size
        new_board.delete_at(new_board.find_index nil)
      end
      
      space_needed = new_board.size - $elem_board.size
      if space_needed > 0
        removals = (new_board - pair).sort_by{|e|[e.name.size, e.name]}
        n_pairs_all = 0
        n_pairs = 0
        print_time_to "plan for the future" do
          obv_removals = removals.select{|e|Future.elem_done? e}
          ww = IO.console.winsize[1]
          Future.each do |npair|
            if obv_removals.size >= space_needed
              puts "#{obv_removals.map(&:name).join ", "} are out of pairs"
              removals = obv_removals.sample space_needed
              break
            end
            
            n_pairs_all += 1
            if npair.any?(&:done)
              obv_removals += npair.select{|e| removals.include?(e) && Future.elem_done?(e)}
              print removals.map(&:name).lossy_inspect(ww).ljust(ww, " ") + "\e[1A"
              next
            end
            n_pairs += 1
            removals.delete npair[0]
            break if removals.size == space_needed
            removals.delete npair[1]
            break if removals.size == space_needed
            
            print removals.map(&:name).lossy_inspect(ww).ljust(ww, " ") + "\e[1A"
          end
        end
        puts "#{n_pairs}/#{n_pairs_all} future pairs condsidered"
        new_board -= removals
      end
      
      tab_width = $elems.map{|e| e ? e.name.length : 0}.max
      additions = new_board - $elem_board
      removals = ($elem_board - new_board).compact.rjust(additions.count)
      removals.zip(additions) do |r, a| 
        puts "#{(r ? r.name : "").rjust(tab_width)} => #{a.name}"
      end
      $elem_board = new_board
    end
    puts out_pair.to_s
    $stats[:pairs_used] += 1
    
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
  Future.initialize
  Future.concat ($elems.combination(2).to_a + $elems.map{|e| [e, e]}).map{|e1, e2| Pair.new(e1, e2)}
  $elems.each{|e| e.tries = 0}
  $elems.each{|e| e.parent_pairs.each{|pp| pop pp}}
end

def p_stats
  puts "elements by pairs left"
  $elems.map{|e|$elems.size - e.tries + 1}.group_by{|x| x}.to_a.sort.reverse.each{|a| p [a[0], a[1].size]}
  puts "elements by outdegree"
  groups = $elems.map(&:successes).group_by{|x| x}
  (0..groups.keys.max||0).each{|k| p [k, groups.fetch(k, []).length]}
  puts "pairs by distance"
  histogram = Future.unordered.map{|pair| pair.reduce(&:distance_to)}.sort.group_by{|x| x}
  (0..histogram.keys.max||0).each{|i| p [i, histogram.fetch(i, []).size]}
  puts "pairs by bitonic distance"
  histogram = Future.unordered.map{|pair| pair.bitonic_length}.sort.group_by{|x| x}
  (0..histogram.keys.max||0).each{|i| p [i, histogram.fetch(i, []).size]}
  
  pairs_total = $elems.count * ($elems.count + 1) / 2
  p elems_open: $elems.count{|e| !e.done}, elems_done: $elems.count{|e| e.done},
    pairs_open: Future.length, pairs_skipped: pairs_total - Future.length - $stats[:pairs_used]

  p $stats
end

def show_elems key, topo_sort = true, successor: nil, nonsuccessors: nil, parent: nil
  sort_orders = {
    name: -> _, _ {""},
    history: -> _, i {i + 1},
    ancestors: -> e, _ {e.ancestors.count},
    outdegree: -> e, _ {e.successes},
    complexity: -> e, _ {e.complexity},
    importance: -> e, _ {$elems.select{|f| f.ancestors.include? e}.count}
  }

  triples = $elems.map.with_index{|e, i| [sort_orders[key.to_sym][e, i], e]}.sort_by{|ix, e| [ix, e.name]}
                  .map{|ix, e| e.parent_pairs.map{|pp| [ix, e, pp]}}.flatten(1)

  succ_elem = $elems.find{|e| e.name == successor}
  raise ArgumentError if successor && !succ_elem
  parent_elem = $elems.find{|e| e.name == parent}
  raise ArgumentError if parent && !parent_elem
  nonsuccessor_elems = nonsuccessors&.map{|ns| $elems.find{|e| e.name == ns} or raise ArgumentError}
  triples.select! {|(_, e, _)| succ_elem == e || succ_elem.ancestors.include?(e)} if succ_elem
  triples.select! {|(_, e, _)| ! nonsuccessor_elems.any?{|ns| ns.ancestors.include?(e)}} if nonsuccessors
  triples.select! {|(_, _, pp)| pp.include?(parent_elem)} if parent_elem

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
    triples.map(&insert)
    triples = triples_sorted
  end
  
  triples.each do |ix, e, pp|
    e.update_text
    puts "@#{ix} #{pp.empty? ? "add" : "#{pp.join " + "}"} = #{e}"
  end
end

def serialize_data word_width = 6, pad = false
  pair_set = Set.new Future.unordered
  bit_packer = BitPacker.new word_width
  live_elems = $elems.reject{|e| Future.elem_done? e}
  unless live_elems.empty? || $elems[live_elems.count - 1] == live_elems.last
    raise "bug: serialize_data requires used up elements to go last"
  end
  live_elems.each_with_index do |e1, i1|
    live_elems[0 .. i1].each{|e2| bit_packer.push_bit(!!pair_set.delete?(Pair.new e2, e1))}
    bit_packer.pad if pad
  end
  bit_packer.zero_pad
  raise "bug: pairs #{pair_set.map{|pair| pair.map(&:name)}} unaccounted for" unless pair_set.empty?
  {
    e: $elems.map{|e| [
         e.name, 
         e.parent_pairs.map{|pair| pair.map{|el| $elems.index el}.compact.sort}.sort, 
         e.done ? 1 : 0
       ]},
    b: $elem_board&.map{|be| be && $elems.index(be)}&.sort_by{|e| [e.nil? ? 1 : 0, e]},
    be: $board_elevator&.get_state,
    st: $stats.values, 
    pt: word_width.to_s + (pad ? "p" : ""),
    p: bit_packer.result[/^(.*[^0])*(?=0*$)/]
  }.compact
end

def deserialize_data data
  (data[:pt] || "6") =~ /^(\d|13)(p?)$/; word_width = $1.to_i; pad = $2 == "p"
  $elems = data[:e].map{|e| el = Elem.new(e[0], []); el.done = e[2] == 1; el}
  data[:e].each do |e|
    elem = $elems.find{|el| el.name == e[0]}
    e[1].each do |ixes|
      pair = ixes.empty? ? [Elem::ROOT] : ixes.map{|ix| $elems[ix]}
      elem.add_pair pair
      pair.each{|p|p.update_text; p.successes += 1; p.children |= [elem]}
    end
  end

  if data[:b]
    $elem_board = data[:b]&.map{|i| i && $elems[i]} #TODO: handle resize
    puts "board size = #{$elem_board.size}"
    board_size_arg = ARGV.find{|arg| arg =~ /--board-size-\d+/}
    if board_size_arg
      puts "board size from arg = #{board_size_arg[/\d+/]}"
      $elem_board << nil while $elem_board.size < board_size_arg[/\d+/].to_i
      removed_elems = $elem_board.slice!(board_size_arg[/\d+/].to_i .. -1).compact
      removed_elems.each{|e| puts "#{e.name} dropped from board"}
    end
    $board_elevator = ElemElevatorEnumerator.new(data[:be])
  end

  Future.initialize
  bit_packer = BitPacker.new word_width, data[:pt].nil?, data[:p]
  $elems.each_with_index do |e1, i1|
    $elems[0 .. i1].each do |e2|
      if bit_packer.shift_bit 
        Future << Pair.new(e2, e1)
      else 
        e1.tries += 1
        e2.tries += 1
      end
    end
    bit_packer.skip_pad if pad
  end

  @categories = $elems.map(&:category).uniq
  [*$elems, Elem::ROOT].each(&:recalc_distances)
  @elems_by_last_seen = $elems.sort_by{|el| [-el.priority, rand]}
  $elems_done = $elems.count(&:done)
  $stats = Hash[$stats_keys.zip(data[:st] || []).map{|k, v| [k, v || 0]}]
end

def mk_save
  best_str = nil
  [false, true].each do |pad|
    [1, 2, 3, 4, 5, 6, 13].each do |word_width|
      data = serialize_data(word_width, pad)
      json = JSON.generate(data)
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
  tail = []
  tries_min = 0
  tries_max = $elems.count + 1
  ww = IO.console.winsize[1]
  loop do
    new_tail = $elems.select{|e| Future.elem_tries(e) == tries_max}
    tries_min += new_tail.size
    tail.unshift(*new_tail)
    puts "new tail: #{new_tail.map(&:name).lossy_inspect(ww - 10)}"
    break if new_tail.empty? && tail.any? || tail.size == $elems.size
    
    new_tail = $elems.select{|e| Future.elem_tries(e) == tries_min}
    tries_max -= new_tail.size
    tail.unshift(*new_tail)
    puts "new tail: #{new_tail.map(&:name).lossy_inspect(ww - 10)}"
    break if new_tail.empty? || tail.size == $elems.size
  end
  puts "elems left: " + ($elems - tail).map{|e| "#{e.name} (#{Future.elem_tries(e)})"}.lossy_inspect(ww - 12)
  $elems = ($elems - tail) + tail
  mk_save
end

###############################################################################

puts "Welcome to Alchemy.rb"
if ARGV.include? "--use-cats"
  $use_cats = true
end
if !ARGV.include? "--lazy-skip"
  $eager_skip = true
  puts "will be skipping eagerly"
end
board_size_arg = ARGV.find{|arg| arg =~ /--board-size-\d+/}
if board_size_arg
  $elem_board = Array.new board_size_arg[/\d+/].to_i
  puts "board size = #{$elem_board.size}"
end

loop do
  begin
    case $stdin.gets
      when /^(.*?)\s*\+\s*(.*?)\s*\=\s*(.*?)$/ # p1+p2=p3 with optional spaces
        r = add($3, [$1, $2]); puts "ok; %s + %s = %s" % r.map(&:to_s) if r
        pop(only_skip: true) if $eager_skip
      when /^add (.*)$/
        pop(only_skip: true) if $eager_skip
        puts "ok; %s" % [add($1, [])[1].to_s]
      when /^\=\s*(.*?)$/
        r = add($1, $last_pair.map(&:name))
        pop(only_skip: true) if $eager_skip
        puts "ok; %s + %s = %s" % r.map(&:to_s) if r
      when /^(pop)?$/ then pop
      when /^done$/ then $elems.each{|e| e.done = true}; pop
      when /^s\/(.*?)\/(.*?)\// then (rename $1, $2; puts "ok")
      when /^remove element (.+)$/ then puts rm_elem $1
      when /^stats$/ then p_stats
      when /^show elements by (.+)$/ then show_elems $1
      when /^show history$/ then show_elems "history"
      when /^next chapter$/ then (reset_pairs; puts "ok")
      when /^show board$/ then puts word_wrap $elem_board&.map{|e|e&.name}&.sort_by(&:inspect).inspect
      when /^show future$/ then puts Future.inspect
      when /^skip (.*)$/
        elem = $elems.find{|e| e.name == $1}
        if elem
          elem.done = true
          puts "ok"
          $elem_board.delete(elem) && $elem_board << nil if $elem_board
          (Future.rewind; pop(only_skip: true)) if $eager_skip
        else
          puts "unknown element #{$1}"
        end
      when /^show history of (.*?)(( - .*?)*)$/
        show_elems "history", successor: $1, nonsuccessors: $2.split(/ - /)[1..-1]
      when /^show children of (.*)$/
        show_elems "history", parent: $1 
      when /^save as (.*)$/
        save = mk_save_opt
        File.rename($1 + ".alchz", $1 + ".alchz.bak") if File.exist?($1 + ".alchz")
        File.open($1 + ".alchz", "wb"){|f| f.write save}
        puts "\e[Gok; #{$elems.length} elements, #{Future.length} pairs"
        puts "wrote #{save.length} bytes to disk"
      when /^load from (.*)$/
        Zlib::GzipReader.open($1 + ".alchz") do |gz|
          json = JSON.parse gz.read, symbolize_names: true
          deserialize_data json
          puts "\e[Gok; #{$elems.length} elements, #{Future.length} pairs"
        end
      when /^(q|quit|exit)$/ then exit
      else puts '?'
    end
  rescue
    puts $!
    puts $@
  end
end
