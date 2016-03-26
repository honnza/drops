require 'json'
require 'zlib'

class CombinationGenerator
  Literal = Struct.new :ix, :is_neg, :for_gen do 
    def to_s; (is_neg ? "-" : "+") + for_gen.names[ix]; end
  end
  class Filter 
    class << self
      def id; @id += 1; end
      attr_writer :id
    end
    def initialize literals, for_gen, id: Filter.id
      @literals = literals.sort_by(&:ix)
      raise ArgumentError, "duplicate name" if @literals.each_cons(2).any? {|pair| pair.first.ix == pair.last.ix}
      @literals_by_ix = {}
      @literals.each {|lit| @literals_by_ix[lit.ix] = lit}
      @id = id
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
          res = Filter.new(@literals - [coDiff], @for_gen, id: self.id)
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
  attr_accessor :names, :filters, :fail_cache

  def add_filter f_names
    learn Filter.new f_names.map {|f_name|
      _, sign, name = *(f_name.match /(.)(.*)/)
      if $custom && ! @names.include?(name)
        ix = @names.length
        @names << name
        puts "new name #{name.inspect}"
      else
        ix = @names.find_index name
      end
      raise ArgumentError, "unknown name #{name.inspect}" unless ix
      Literal.new ix, sign == "-", self
    }, self
    @filters.sort_by! &:id

    @filters.join ", "
  end

  def serialize
    keys_map = Hash[@filters.map.with_index(1){|f, ix|[f.id, ix]}]
    {
      n: @names.dup,
      f: @filters.map{|f|f.literals.map{|l|l.is_neg ? ~l.ix : l.ix}},
      c: @fail_cache.keys.map{|fs, len, ones| [fs.map{|f|keys_map[f]}, len, ones]}
    }
  end

  def self.deserialize o
    gen = new o[:n]
    o[:f].each do |f|
      gen.learn Filter.new f.map{|l|
        if l < 0
          Literal.new ~l, true, gen
        else
          Literal.new l, false, gen
        end
      }, gen
    end
    if o[:c]
      o[:c].each{|k| gen.fail_cache[k] = true}
    end
    gen
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
        @filters.delete f
        @fail_cache.reject! {|k, _v| k.include? f.id}
      end
      to_learn << difff if difff && difff != f
    end

    @filters.push nf
    to_learn.each {|f| learn f}
  end

  def rm_filter
    @filters.pop
  end

  def next; @enum.next; end

  def make_enum
    @enum = Enumerator.new do |y|
      loop.with_index do |_, ones|
        break if ones > @names.length 
        do_enum(y, "", ones)
      end
    end
  end

  def do_enum y, path, ones
    puts path if path.length < 160
    sleep $speed if $speed > 0
    unsat_filters = @filters.reject{|f| f.satisfied_by? path};
    yield_clock = @yield_clock
    return if unsat_filters.any? {|f| f.last_ix < path.length}
    if path.length == @names.length
      y.yield path.chars.map.with_index {|c, ix| c == '1' ? @names[ix] + " " : ""}.join ""
      @yield_clock += 1 
    end

    some_unsat_filters = unsat_filters.map(&:id);
    some_unsat_filters.pop until some_unsat_filters.empty? || @fail_cache[[some_unsat_filters, path.length, ones]]
    
    unless @fail_cache[[some_unsat_filters, path.length, ones]]
      do_enum(y, path + "0", ones) if ones + path.length < @names.length
      do_enum(y, path + "1", (ones - 1)) if ones > 0
      if yield_clock == @yield_clock
        @fail_cache[[unsat_filters.map(&:id), path.length, ones]] = true
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

class SolutionTracker
  public def initialize
    @n_solutions = 0
    @lit_stats = Hash.new {|hash, key| hash[key] = 0}
    @speculation = nil
  end

  attr_accessor :speculation
  def add solution
    @n_solutions += 1
    solution.split.each{|name| @lit_stats[name] += 1}
  end
  def add_speculation
    add @speculation if @speculation
    @speculation = nil
  end
  def drop_speculation
    @speculation = nil
  end

  def stats
    "#{@n_solutions} solutions total\n" +
    @lit_stats.to_a.sort_by(&:reverse).inspect
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
  not: ->n, names{names.combination(n).map{|m_names|minuses(m_names) + pluses(names - m_names)}},
  just: ->n, names{$macros[:max][n, names] + $macros[:min][n, names]}
}

def run_macro macro, n, names
  $macros[macro][n, names].each do |f_names|
    puts "   #{f_names.join " "}"
    make_filter f_names.join " "
  end
end

def do_command line
  case line
    when /^new custom$/
      $undo_log.push [$enum.serialize, line] if $enum
      $solution_tracker.initialize
      $custom = true
      $enum = CombinationGenerator.new []
      $renderer = nil
      $new_str = line
      puts "ok; custom names"
    when /^new (\w+)(.+)$/
      $undo_log.push [$enum.serialize, line] if $enum
      $solution_tracker.initialize
      $custom = false
      $new_str = line
      puts "ok; #{make_gen $1, $2}"
    when /^([+-]\w+( |\b))+\.$/ 
      $undo_log.push [$enum.serialize, line]
      $solution_tracker.drop_speculation
      puts "ok; #{make_filter $&.chop}"
    when /^pop$/ 
      $solution_tracker.add_speculation
      sol = $enum.next
      puts $renderer[sol, $enum] if $renderer
      puts rolling_prefix "pop", sol
      $solution_tracker.speculation = sol
    when /^undo$/ 
      if $undo_log.empty?
        puts "nothing to undo"
      else
        $solution_tracker.initialize
        save, line = $undo_log.pop
        $enum = CombinationGenerator.deserialize save
        puts "undone #{line.chop}; #{$enum.filters.join ' '}"
      end
    when /^recount$/ then $solution_tracker.initialize; $enum.make_enum; puts "ok"
    when /^retrain$/ then puts $enum.retrain
    when /^stats$/
      puts $enum.stats
      puts $solution_tracker.stats
    when /^save as (.*)$/
      json = JSON.generate({n: $new_str, e: $enum.serialize})
      Zlib::GzipWriter.open($1 + ".wvrz") {|gz| gz.write json}
      zip_size = File.size $1 + ".wvrz"
      pct_string = "%.2f" % (zip_size.to_f / json.size * 100)
      puts "ok; #{zip_size} bytes written (#{pct_string}% compression ratio)"
    when /^load from (.*)$/
      Zlib::GzipReader.open($1 + ".wvrz") do |gz|
        json = JSON.parse gz.read, symbolize_names: true
        do_command json[:n]
        $enum = CombinationGenerator.deserialize json[:e]
        puts "ok; #{$enum.filters.join ' '}"
        $undo = []
      end
    when /^exit$|^quit$|^q$/ then exit

    when /^pop level$/
      if $solution_tracker.speculation
        level = $solution_tracker.speculation.split.length
        while $solution_tracker.speculation.split.length == level
          $solution_tracker.add_speculation
          sol = $enum.next
          puts $renderer[sol, $enum] if $renderer
          puts rolling_prefix "pop", sol
          $solution_tracker.speculation = sol
        end        
      else
        puts "unknown which level to pop"
      end

    when /^(\w+) (\d+) of ([\w\s]+)\./
      $undo_log.push [$enum.serialize, line]
      $solution_tracker.drop_speculation
      names = $3.split(" ")
      puts "unknown macro #{$1}" unless $macros[$1.to_sym]
      run_macro $1.to_sym, $2.to_i, $3.split(" ")
      puts $enum.filters.join " "
    else puts "?"
  end
end 

puts "Welcome to Weaver.rb"
$solution_tracker = SolutionTracker.new
$undo_log = [];
loop do
  $undo_log.shift if $undo_log.length > 100
  begin
    putc ?>
    line = gets.chomp
    do_command line
  rescue 
    puts $!
    puts $!.backtrace
  end
end