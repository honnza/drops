class CombinationGenerator
  Literal = Struct.new :ix, :is_neg do 
    def to_s; (is_neg ? "-" : "+") + ix.to_s; end
  end
  class Filter 
    def initialize literals
      @literals = literals.sort_by(&:ix)
      raise ArgumentError, "duplicate name" if @literals.each_cons(2).any? {|pair| pair.first.ix == pair.last.ix}
      @literals_by_ix = {}
      @literals.each {|lit| @literals_by_ix[lit.ix] = lit}
    end
    attr_accessor :literals

    def satisfied_by? str
      @literals.any? {|lit| str[lit.ix] == (lit.is_neg ? "0" : "1")}
    end

    def last_ix; @literals.last.ix; end
    def [] ix; @literals_by_ix[ix]; end
    def to_s; "Filter{#{@literals.map(&:to_s).join(", ")}}"; end
  end

  def initialize names
    @names = names
    @filters = []
    make_enum 
    @fail_cache = {}
    @yield_clock = 0
  end

  def add_filter f_names
    @filters.push Filter.new f_names.map {|f_name|
      _, sign, name = *(f_name.match /(.)(.*)/)
      ix = @names.find_index name
      raise ArgumentError, "unknown name #{name.inspect}" unless ix
      Literal.new ix, sign == "-"
    }
    "#{@filters.length} filters, last #{@filters.last}"
  end

  def rm_filter
    @filters.pop
  end

  def next; @enum.next; end

  def make_enum
    @enum = Enumerator.new do |y|
      (0 .. @names.length).each do |ones| 
        do_enum(y, "", ones, @names.length - ones)
      end
    end
  end

  def do_enum y, path, ones, zeroes
    puts path
    sleep $speed if $speed > 0
    unsat_filters = @filters.reject {|f| f.satisfied_by? path}
    yield_clock = @yield_clock
    return if unsat_filters.any? {|f| f.last_ix < path.length}
    if path.length == @names.length
      y.yield path.chars.map.with_index {|c, ix| c == '1' ? @names[ix] + " " : ""}.join ""
      @yield_clock += 1 
    end

    unless @fail_cache[[unsat_filters, ones, zeroes]]
      do_enum(y, path + "0", ones, (zeroes - 1)) if zeroes > 0
      do_enum(y, path + "1", (ones - 1), zeroes) if ones > 0
      if yield_clock == @yield_clock
        @fail_cache[[unsat_filters, ones, zeroes]] = true
      end
    end
  end
end

def make_gen x, y
  raise ArgumentError, "x > 26" if x > 26
  raise ArgumentError, "y > 10" if y > 10

  names = (?a..?z).take(x).flat_map do |i|
    (?0..?9).take(y).map do |j|
      i+j
    end
  end

  $enum = CombinationGenerator.new names

  names.first .. names.last
end

def make_filter neg, pos
  neg=neg.split.map{|neg|"-" + neg}
  pos=pos.split.map{|pos|"+" + pos}
  $enum.add_filter neg+pos
end

$speed = 0
puts "Welcome to Weaver.rb"
loop do
  begin
    putc ?>
    case gets
      when /^new (\d+)x(\d+)$/ then puts "ok; #{make_gen $1.to_i, $2.to_i}"
      when /^(.*)\|(.*)$/ then puts "ok; #{make_filter $1, $2}"
      when /^pop$/ then puts $enum.next
      when /^undo$/ then $enum.rm_filter; puts "ok"
      when /^recount$/ then $enum.make_enum; puts "ok"
      when /^speed\s?=\s?([\d.]+)$/ then $speed = $1.to_f; puts "ok"
      when /^exit$|^quit$|^q$/ then exit
      else puts "?"
    end
  rescue 
    puts $!
    puts $!.backtrace
  end
end