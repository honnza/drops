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
      str_capitals = str.count "A-Z"
      str_minuscules = str.count "a-z"
      str_symbols = str.length - str_capitals - str_minuscules
      p [str, :all, str_symbols, str_minuscules, str_capitals] unless @recursive_mode
      ([(str.length-1 if @slow_mode), [@min_chars, str.length].min].compact.max .. str.length).each do |sublen|
        enum = Enumerator.new do |y2|
          vals_uniq = Set.new
          vals_total = 0
          str.chars.combination(sublen).each do |sub|
            y2.yield(sub) unless @count_uniq && vals_uniq.include?(sub)
            vals_uniq << sub if @count_uniq && @sort != [:caps_first, :ix]
            vals_total += 1
          end
          if @count_uniq
            p [str, sublen, "#{vals_uniq.count}/#{vals_total}"]
          end
        end
        case @sort
        when [:ix] then nil
        when [:caps_first, :ix]
          length_enum = enum
          enum = Enumerator.new do |y2|
            0.upto([str_symbols, sublen].min) do |sub_symbols|
              0.upto([str_minuscules, sublen - sub_symbols].min) do |sub_minuscules|
                sub_capitals = sublen - sub_minuscules - sub_symbols
                if sub_capitals <= str_capitals
                  vals_uniq = Set.new
                  vals_total = 0
                  length_enum.each do |sub|
                    if sub.join.count("A-Z") == sub_capitals && sub.join.count("a-z") == sub_minuscules
                      y2.yield(sub) unless @count_uniq && vals_uniq.include?(sub)
                      vals_uniq << sub if @count_uniq
                      vals_total += 1
                    end
                  end
                  if @count_uniq
                    p [str, sub_symbols, sub_minuscules, sub_capitals, "#{vals_uniq.count}/#{vals_total}"]
                  end
                end
              end
            end
          end
        else
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
          enum.each{|c|puts "#{highlight_diff(str, c.join, "")}\e[1A" if rand < 1e-4; y.yield c.join}
        when :all
          y.yield enum.to_a.map(&:join)#.uniq.sort
        else
          raise "unknown tiebreak #{@tiebreak.inspect}"
        end
      end
    end
  end

  attr_reader :strs
  def initialize(letter_only: false, tiebreak: :first, slow_mode: false, sort: nil,
                 count_uniq: false, redundant_mode: false, min_chars: 0,
                 recursive_mode: false, reverse_regex: false)
    @strs = []
    @enums = []
    @tiebreak = tiebreak
    @letter_only = letter_only
    @slow_mode = slow_mode
    @sort = [*sort] | [:ix]
    @count_uniq = count_uniq
    @redundant_mode = redundant_mode
    @min_chars = min_chars
    @recursive_mode = recursive_mode
    @result_cache = []
    @reverse_regex = reverse_regex

    @regexp_rev_cache = Hash.new
  end
  
  def dup_empty
    IdentSubseqCalc.new(letter_only: @letter_only, tiebreak: @tiebreak,
      slow_mode: @slow_mode, sort: @sort, count_uniq: @count_uniq, redundant_mode: @redundant_mode,
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
  
  def regexp_for k, redundancy
    sub_regexps = k.chars.combination(k.size - redundancy).map do |ks|
      ks.map{|c| Regexp.escape c}.join(".*")
    end
    Regexp.new(sub_regexps.join("|"), (//i if @letter_only))
  end
  
  def regexp_rev k, redundancy
    @regexp_rev_cache[[k, redundancy]] ||= begin
      substr_bits = k.chars.map{|c| Regexp.escape(c) + "?"}
      regexp_str = case redundancy
      when 0 then substr_bits.join
      when 1
        #produces string in the form of .f?o?o?|f?.o?o?|f?o?.o?|f?o?o?.
        (0..k.length).map{|ix| substr_bits.dup.insert(ix, ".?").join}.join "|"
      else raise "redundancy > 1 not implemented for reverse regex strategy"
      end
      Regexp.new "^(#{regexp_str})$"
    end
  end

  def result ix
    ix = strs.find_index ix if ix.is_a? String
    str = strs[ix]
    enum = @enums[ix]
    str = str.gsub(/[^a-zA-Z]/, "") if @letter_only
    strRegex = regexp_for str, 0
    strRegex2 = regexp_for str, 1
    conflicts = @strs.reject {|s2| s2 =~ strRegex}
    conflicts2 = @strs.reject {|s2| s2 =~ strRegex2}
    new_conflicts = @strs.drop(@result_cache.size) & conflicts
    new_conflicts2 = @strs.drop(@result_cache.size) & conflicts2

    filter = if @redundant_mode && @reverse_regex
      conflicts -= conflicts2
      new_conflicts -= new_conflicts2
      lambda do |str|
        if @result_cache[ix] == str
          new_conflicts2.any? {|s2| str =~ regexp_rev(s2, 1)} || new_conflicts.any? {|s2| str =~ regexp_rev(s2, 0)}
        else
          conflicts2.any? {|s2| str =~ regexp_rev(s2, 1)} || conflicts.any? {|s2| str =~ regexp_rev(s2, 0)}
        end
      end
    elsif @reverse_regex
      lambda do |str|
        if @result_cache[ix] == str
          new_conflicts.any? {|s2| str =~ regexp_rev(s2, 0)}
        else
          conflicts.any? {|s2| str =~ regexp_rev(s2, 0)}
        end
      end
    elsif @redundant_mode
      conflicts -= conflicts2
      new_conflicts -= new_conflicts2
      lambda do |str|
        strRegex = regexp_for str, 0
        strRegex2 = regexp_for str, 1
        if @result_cache[ix] == str
          new_conflicts2.any? {|s2| s2 =~ strRegex2} || new_conflicts.any? {|s2| s2 =~ strRegex}
        else
          conflicts2.any? {|s2| s2 =~ strRegex2} || conflicts.any? {|s2| s2 =~ strRegex}
        end
      end
    else
      lambda do |str|
        strRegex = regexp_for str, 0
        if @result_cache[ix] == str
          new_conflicts.any? {|s2| s2 =~ strRegex}
        else
          conflicts.any? {|s2| s2 =~ strRegex}
        end
      end
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
  
  def results; @result_cache = (0...strs.count).map{|i| result i}; end
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
  end.join.gsub(" ", "•")
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
  count_uniq = ARGV.include?("--count-uniq")
  reverse_regex = ARGV.include?("--rer")

  suppress_output = false
  
  puts "warning: -s is best used with -r" if slow_mode && !recursive_mode
  
  if tiebreak == :all && recursive_mode
    puts "error: -a is incompatible with -r"
    exit
  end

  calc = IdentSubseqCalc.new(tiebreak: tiebreak, letter_only: letter_only, sort: sort, count_uniq: count_uniq,
                             slow_mode: slow_mode, redundant_mode: redundant_mode, min_chars: min_chars, 
                             recursive_mode: recursive_mode, reverse_regex: reverse_regex)
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
