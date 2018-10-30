# encoding: UTF-8

require 'io/console'
require 'set'
require 'etc'

class Array
  def inspect
    "##{to_s}(#{length})"
  end
end

class IdentSubseqCalc
  def subseq_enum str
    Enumerator.new do |y|
      ([(str.length-1 if @slow_mode), @min_chars].compact.max .. str.length).each do |sublen|
        enum = str.chars.combination(sublen)
        if @sort != [:ix]
          enum = enum.sort_by.with_index do |elem, ix| 
            [*@sort].map do |key|
              case key
              when :caps_first
                [elem.join.count("^a-zA-Z"), elem.join.count("a-z")]
              when :alpha then elem
              when :ix then ix
              else raise "unknown sort order #{key.inspect}"
              end
            end
          end
        end
        case @tiebreak
        when :first
          enum.each{|c|puts "#{highlight_diff(str, c.join, "")}\eK\eA" if rand < 0.001; y.yield c.join}
        when :all
          y.yield enum.to_a.map(&:join)#.uniq.sort
        else
          raise "unknown tiebreak #{@tiebreak.inspect}"
        end
      end
    end
  end

  attr_reader :strs
  def initialize(letter_only: false, tiebreak: :first, slow_mode: false, sort: nil, redundant_mode: false, min_chars: 0)
    @strs = []
    @enums = []
    @tiebreak = tiebreak
    @letter_only = letter_only
    @slow_mode = slow_mode
    @sort = [*sort] | [:ix]
    @redundant_mode = redundant_mode
    @min_chars = min_chars
  end
  
  def dup_empty
    IdentSubseqCalc.new(letter_only: @letter_only, tiebreak: @tiebreak,
      slow_mode: @slow_mode, sort: @sort, redundant_mode: @redundant_mode,
      min_chars: @min_chars)
  end

  def push str
    str = str.downcase if @letter_only
    return false if @strs.include? str
    @strs << str
    if @letter_only
      @enums << subseq_enum(str.gsub(/[^a-z]/, ""))
    else
      @enums << subseq_enum(str)
    end
    true
  end
  
  def push_all strs
    strs.each{|str| push str}
    self
  end
  
  def regexp_for k
    redundancy = @redundant_mode ? 1 : 0
    sub_regexps = k.chars.combination(k.size - redundancy).map do |ks|
      ks.map{|c| Regexp.escape c}.join(".*")
    end
    Regexp.new(sub_regexps.join("|"), (//i if @letter_only))
  end

  def result ix
    ix = strs.find_index ix if ix.is_a? String
    str = strs[ix]
    enum = @enums[ix]
    str = str.gsub(/[^a-zA-Z]/, "") if @letter_only
    strRegex = regexp_for str
    conflicts = @strs.reject {|s2| s2 =~ strRegex}

    filter = lambda do |str|
      strRegex = regexp_for str
      conflicts.any? {|s2| s2 =~ strRegex}
    end

    enum.next unless enum.peek
    case @tiebreak
    when :first
      enum.next while filter[enum.peek]
      enum.peek
    when :all
      enum.next while enum.peek.all? &filter
      enum.peek.reject! &filter #here, we mutate a peek result for side effects. Sorry!
      enum.peek.dup
    else
      raise "unknown tiebreak #{@tiebreak.inspect}"
    end
  end
  
  def lenthen_result ix
    ix = strs.find_index ix if ix.is_a? String
    case @tiebreak
    when :first
      cur_size = result(ix).size
      return if cur_size == @strs[ix].size
      @enums[ix].pop until @enums[ix].peek.size > cur_size
    when :all
      @enums[ix].replace []
    else
      raise "unknown tiebreak #{@tiebreak.inspect}"
    end
  end
  
  def results; (0...strs.count).map{|i| result i}; end
end

################################################################################

class RecursiveSubseqCalc
  def initialize calc, opts
    @opts = opts
    @slow_mode = opts[:slow_mode]
    @animation_mode = opts[:animation_mode]
    @top = calc
  end
  
  def strs; @top.strs; end
  def push str; @top.push str; end
  
  def results
    strs = @slow_mode ? @top.strs : @top.results
    loop do
      calc = @top.dup_empty.push_all(strs)
      (puts "", strs; sleep 0.1) if @animation_mode
      ml = strs.map(&:length).max
      if @slow_mode
        r = strs.map.with_index{|str, ix| str.length == ml ? calc.result(ix) : str}
      else
        r = calc.results
      end
      return r if r == strs
      strs = r
    end
  end
end

################################################################################

def find_subseq haystack, needle
  last_ix = -1
  needle.chars.map{|c| last_ix = haystack.index(c, last_ix + 1)}
end

def highlight_diff in_str, new_str, old_str
  new_ixes = find_subseq in_str, new_str if new_str.is_a? String
  old_ixes = find_subseq in_str, old_str if old_str.is_a? String
  
  in_str.chars.map.with_index do |c, ix|
    color = case [new_ixes.include?(ix), old_ixes.include?(ix)]
            when [false, false] then "30;1"   # light gray
            when [false, true]  then "31;1" # bright red
            when [true, false]  then "32;1" # bright green
            when [true, true]   then "33;1" # bright yellow
            end
    "\e[#{color}m#{c}\e[0m"
  end.join
end

################################################################################

if $0 == __FILE__
  sort = ARGV.include?("-a") ? [:alpha] : []
  sort.unshift :caps_first if ARGV.include?("-c")
  redundant_mode = ARGV.include?("-d")
  show_histogram = ARGV.include?("-h")
  min_chars = ARGV.find{|arg| arg.start_with? "-m"}
  min_chars = min_chars ? min_chars[2..-1].to_i : 0
  animation_mode = ARGV.include?("-ra")
  recursive_mode = ARGV.include?("-r") || animation_mode
  slow_mode = ARGV.include?("-s")
  show_time = ARGV.include?("-t")
  letter_only = ARGV.include?("-w")
  tiebreak = ARGV.include?("--all") ? :all : :first

  suppress_output = false
  
  puts "warning: -s is best used with -r" if slow_mode && !recursive_mode
  
  if tiebreak == :all && recursive_mode
    puts "error: -a is incompatible with -r"
    exit
  end

  calc = IdentSubseqCalc.new(tiebreak: tiebreak, letter_only: letter_only, sort: sort, 
                             slow_mode: slow_mode, redundant_mode: redundant_mode, min_chars: min_chars)
  calc = RecursiveSubseqCalc.new(calc, animation_mode: animation_mode, slow_mode: slow_mode) if recursive_mode
                
  results = []
  td_cummulative = 0
  loop do
    print suppress_output ? "> " : ">"
    str = $stdin.gets.chomp
    break if str.empty?
    case str
      when "{" then suppress_output = true; next
      when "}" then suppress_output = false; dup_input = false
      when %r{^\?/(.*)/$}
        re = Regexp.new $1
        calc.strs.zip(calc.results).sort.each{|str, result| puts "#{str.inspect} => #{result.inspect}" if str =~ re}
      else dup_input = !calc.push(str)
    end
      
    if suppress_output
      nil
    elsif dup_input
      puts "duplicate string"
    else
      
      time_start = Time.now
      old_results = results
      results = calc.results
      time_delta = Time.now - time_start
      td_cummulative += time_delta
            
      calc.strs.zip(old_results, results).sort.each do |in_str, old_str, new_str|
        if old_str == new_str
          nil
        elsif tiebreak == :all && old_str && new_str[0].length == old_str[0].length
          puts "- %p = %p | %s" % [old_str - new_str, new_str, in_str]
        elsif tiebreak == :all
          puts "%p => %p | %s" % [old_str, new_str, in_str]
        else
          puts "%p => %p | %s" % [old_str || "", new_str, highlight_diff(in_str, new_str, old_str || "")]
        end
      end
 
      count_hsh = results.map{|e| tiebreak == :all ? e[0].length : e.length}.group_by(&:itself)
      counts = (count_hsh.keys.min .. count_hsh.keys.max).map{|k|count_hsh.fetch(k, []).length}
      scale = (IO.console.winsize[1] - 1) / Math.log(counts.max + 1)
      p counts if show_histogram
      if show_histogram && tiebreak == :all
        p (count_hsh.keys.min .. count_hsh.keys.max)
            .map{|k| results.reduce(0){|a, d| d[0].length == k ? a + d.length : a}}
      end
      counts.each{|c| puts "*" * (Math.log(c + 1) * scale)}
      puts "%.6fs (%.6fs)" % [td_cummulative, time_delta] if show_time
      
      print "\a" if time_delta > 60
    end
  end
  results = calc.results
  if tiebreak == :all
    calc.strs.zip(results).sort.each{|full| puts full.map(&:inspect).join(" => ")}
  else
    offset = results.map(&:length).max
    calc.strs.zip(results).sort.each{|in_str, out_str| puts "%#{offset}s %s" % [out_str, in_str]}
  end
end
