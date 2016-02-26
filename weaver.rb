class CombinationGenerator
  Literal = Struct.new :ix, :is_neg, :for_gen do 
    def to_s; (is_neg ? "-" : "+") + for_gen.names[ix]; end
  end
  class Filter 
    class << self; attr_accessor :id; end
    def initialize literals, for_gen
      @literals = literals.sort_by(&:ix)
      raise ArgumentError, "duplicate name" if @literals.each_cons(2).any? {|pair| pair.first.ix == pair.last.ix}
      @literals_by_ix = {}
      @literals.each {|lit| @literals_by_ix[lit.ix] = lit}
      @id = Filter.id
      Filter.id += 1
      @for_gen = for_gen
    end
    attr_accessor :literals, :id

    def satisfied_by? str
      @literals.any? {|lit| str[lit.ix] == (lit.is_neg ? "0" : "1")}
    end

    def - other
      diff = other.literals - @literals
      case diff.size
      when 0 
        puts "#{other} => #{self}"
        nil
      when 1
        coDiff = @literals.find{|lit|lit.ix == diff[0].ix}
        if coDiff.nil?
          self
        else
          res = Filter.new(@literals - [coDiff], @for_gen)
          puts "#{self} - #{other} = #{res}"
          res
        end
      else self
      end
    end

    def last_ix; @literals.last.ix; end
    def [] ix; @literals_by_ix[ix]; end
    def to_s; "##{@id}{#{@literals.map(&:to_s).join(" ")}}"; end
  end

  def initialize names
    @names = names
    @filters = []
    make_enum 
    @fail_cache = {}
    @yield_clock = 0
    Filter.id = 0
  end
  attr_accessor :names, :filters

  def add_filter f_names
    learn Filter.new f_names.map {|f_name|
      _, sign, name = *(f_name.match /(.)(.*)/)
      ix = @names.find_index name
      raise ArgumentError, "unknown name #{name.inspect}" unless ix
      Literal.new ix, sign == "-", self
    }, self
  end
  def learn nf
    loop do
      onf = nf
      @filters.each{|f| nf -= f; return "new filter redundant" unless nf}
      break if onf == nf
    end

    to_learn = []
    @filters.dup.each do |f|
      difff = f - nf
      if difff != f
        size = @fail_cache.size
        @filters.delete f
        @fail_cache.reject! {|k, _v| k.include? f.id}
      end
      to_learn << difff if difff && difff != f
    end

    @filters.push nf
    to_learn.each {|f| learn f}

    @filters.join ", "
  end

  def rm_filter
    filter = @filters.pop
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
    unsat_filters = @filters.reject{|f| f.satisfied_by? path};
    yield_clock = @yield_clock
    return if unsat_filters.any? {|f| f.last_ix < path.length}
    if path.length == @names.length
      y.yield path.chars.map.with_index {|c, ix| c == '1' ? @names[ix] + " " : ""}.join ""
      @yield_clock += 1 
    end

    some_unsat_filters = unsat_filters.map(&:id);
    some_unsat_filters.pop until some_unsat_filters.empty? || @fail_cache[[some_unsat_filters, ones, zeroes]]
    
    unless @fail_cache[[some_unsat_filters, ones, zeroes]]
      do_enum(y, path + "0", ones, (zeroes - 1)) if zeroes > 0
      do_enum(y, path + "1", (ones - 1), zeroes) if ones > 0
      if yield_clock == @yield_clock
        @fail_cache[[unsat_filters.map(&:id), ones, zeroes]] = true
      end
    end
  end

  def retrain
    filters = @filters
    @filters = []
    filters.each{|f| learn f}
    @filters.join ", "
  end

  def stats
    histogram = @filters.group_by{|f|f.literals.length}.map{|k,v|[k,v.length]}
    bois = histogram.map do |k, v|
      expK = 2 ** -k
      - v * Math.log2(1-expK)
    end
    "#{histogram} #{bois} #{bois.reduce 0, :+}"
  end
end

def make_gen type, dimStr
  dimStrs = dimStr.split 'x'
  dims = dimStrs.map &:to_i

  case type
  when "cells"
    raise ArgumentError, "new ?" if dims.size != 2
    raise ArgumentError, "x > 26" if dims[0] > 26
    raise ArgumentError, "y > 10" if dims[1] > 10
    names = (?a..?z).take(dims[0]).flat_map do |i|
      (?0..?9).take(dims[1]).map do |j|
        i+j
      end
    end
    $renderer = lambda do |sol, enum|
      r_names = sol.split
      (?a..?z).take(dims[0]).flat_map do |i|
        (?0..?9).take(dims[1]).map do |j|
          (r_names.include? i+j) ? "#" : enum.filters.any?{|f| f.literals.any? {|l| names[l.ix] == i+j}} ? "." : " "
        end.join " "
      end.join "\n"
    end
  when "edges"
    raise ArgumentError, "new ?" if dims.size != 2
    raise ArgumentError, "x > 26" if dims[0] > 26
    raise ArgumentError, "y > 10" if dims[1] > 10
    names = (?a..?z).take(dims[0]).each_cons(2).flat_map do |i1, i2|
      (?0..?9).take(dims[1]).map do |j|
        i1+i2+j
      end
    end + (?a..?z).take(dims[0]).flat_map do |i|
      (?0..?9).take(dims[1]).each_cons(2).map do |j1, j2|
        i+j1+j2
      end
    end
    names.sort_by! {|x| [x[0], x[/\d/], x]}
    $renderer = lambda do |sol, enum|
      names = sol.split
      vedges = (?a..?z).take(dims[0]).each_cons(2).flat_map do |i1, i2|
        (?0..?9).take(dims[1]).map do |j|
          (names.include? i1+i2+j) ? "#" : "."
        end.join "  "
      end
      hedges = (?a..?z).take(dims[0]).flat_map do |i|
        (?0..?9).take(dims[1]).each_cons(2).map do |j1, j2|
          (names.include? i+j1+j2) ? " ##" : " .."
        end.join ""
      end
      hedges.zip(vedges).map{|h, v| v ? "#{h}\n#{v}\n#{v}" : h}.join "\n"
    end
  else
    raise ArgumentError, "new ?"
  end

  $enum = CombinationGenerator.new names

  names.join " "
end

def make_filter str
  $enum.add_filter str.split
end

$rolling_prefix_data = Hash.new{""}

def rolling_prefix scope, str
  prefix = $rolling_prefix_data[scope].dup
  prefix.chop! until str.start_with? prefix
  $rolling_prefix_data[scope] = str
  str.sub prefix, prefix + "|"
end

$speed = 0

def minuses names; names.map{|n|"-#{n}"}; end
def pluses names; names.map{|n|"+#{n}"}; end

$macros = {
  max: ->n, names{names.combination(n+1).map{|names|minuses names}},
  min: ->n, names{names.combination(names.length-n+1).map{|names|pluses names}},
  not: ->n, names{names.combination(n).map{|m_names|minuses(m_names) + pluses(names - m_names)}}
}

def run_macro macro, n, names
  $macros[macro][n, names].each do |f_names|
    puts "   #{f_names.join " "}"
    make_filter f_names.join " "
  end
end

puts "Welcome to Weaver.rb"
last_sol = nil
lit_stats = nil
sols = 0
loop do
  begin
    putc ?>
    case gets
      when /^new (\w+)(.+)$/
        lit_stats = Hash.new {|hash, key| hash[key] = 0}
        last_sol = nil
        sols = 0
        puts "ok; #{make_gen $1, $2}"
      when /^([+-]\w+( |\b))+\.$/ then last_sol = nil; puts "ok; #{make_filter $&.chop}"
      when /^pop$/ 
        sol = $enum.next
        puts $renderer.(sol, $enum) if $renderer
        puts rolling_prefix "pop", sol
        last_sol.split.each{|name| lit_stats[name] += 1} if last_sol
        sols += 1 if last_sol
        last_sol = sol
#     when /^undo$/ then $enum.rm_filter; puts "ok"  # TODO: fix
      when /^recount$/ then $enum.make_enum; puts "ok"
      when /^retrain$/ then puts $enum.retrain
      when /^stats$/
        puts $enum.stats
        puts "#{sols} solutions total"
        puts lit_stats.to_a.sort_by(&:reverse).inspect
      when /^exit$|^quit$|^q$/ then exit

      when /^pop level$/
        if last_sol
          level = last_sol.split.length
          while last_sol.split.length == level
            sol = $enum.next
            last_sol.split.each{|name| lit_stats[name] += 1} if last_sol
            sols += 1 if last_sol
            last_sol = sol
          end        
        else
          puts "unknown which level to pop"
        end

      when /^(\w+) (\d+) of ([\w\s]+)\./
        names = $3.split(" ")
        puts "unknown macro #{$1}" unless $macros[$1.to_sym]
        run_macro $1.to_sym, $2.to_i, $3.split(" ")
        puts $enum.filters.join " "
      else puts "?"
    end
  rescue 
    puts $!
    puts $!.backtrace
  end
end