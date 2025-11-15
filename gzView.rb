require "io/console"
require 'base64'

def to_bytes_str(bytes)
  bytes.map{|byte| byte.to_s(16).rjust(4, "0x00") }.join " "
end

class Array
  def bytes_to_int
    reverse.reduce(0){|a,b| 256*a+b}
  end
  def bits_to_int
    reverse.reduce(0){|a,b| 2*a+b}
  end
  def rtake(n)
    self[-n ...] || self
  end
end

class String
  def display_length; gsub(/\e.*?m/,"").length; end
  def ljust_d(len); self + " " * [len - display_length, 0].max; end
  def bytes_to_glyphs
    self.force_encoding(Encoding::UTF_8)
        .chars.map do |c|
          case 
          when c == "\0" then "\e[7m \e[27m"
          when !$binary && c == " " then "•"
          when !$binary && c.valid_encoding? && c.ord > 0x20 then c
          else 
            bits = c.bytes[0]
            dots =  bits[7] << 0 | bits[3] << 3 |
                    bits[6] << 1 | bits[2] << 4 |
                    bits[5] << 2 | bits[1] << 5 |
                    bits[4] << 6 | bits[0] << 7
            (0x2800 + dots).chr(Encoding::UTF_8)
          end
    end
  end

  def take(n)
    self[... n] || self
  end
  def rtake(n)
    self[-n ...] || self
  end
end

class BitReader
  def initialize io, base64:
    @io = io
    @bits_read = 0
    @buffer = []
    @base64 = base64
    @bytes_read_strs = []
  end
  
  def start_random!
    @io = Object.new
    def @io.read(n)
      if n == 1
        rand 256
      else
        n.times.map{rand 256}
      end
    end
  end
  
  def p_byte b; @bytes_read_strs << "\e[30;1m%02X\e[0m" % b; b; end
  def pop_bytes_read_str
    r = @bytes_read_strs.join " "
    @bytes_read_strs = []
    r
  end

  def peek_bits(n)
    while @buffer.size < n
      chars = if @base64
        Base64.decode64(@io.read(4))
      else
        @io.read(1)
      end
      chars.each_byte{|b| p_byte b;(0..7).map{|i| @buffer.push b[i]}}
    end
    
    @buffer.take(n)
  end
  
  def get_bits(n)
    r = peek_bits(n)
    @bits_read += n
    @buffer.shift(n)
    r
  end
    
  def get_bytes(n, is_peek: false)
    pad = -@bits_read % 8
    bits = is_peek ? peek_bits(8 * n + pad) : get_bits(8 * n + pad)
    bits.drop(pad).each_slice(8).map(&:bits_to_int)
  end
  
  def peek_bytes(n); get_bytes(n, is_peek: true); end
  
  def read_asciiz
    bytes = []
    bytes << get_bytes(1)[0] until bytes.last == 0
    bytes[0 .. -2].pack("U*")
  end

  def read_huffman(huffman_code)
    key = ""
    key += get_bits(1)[0].to_s until huffman_code[key]
    [key, huffman_code[key]]
  end
end

################################################################################

class HuffmanCode
  def initialize codes
    @codes = codes
  end
  
  attr_reader :codes
  
  def [](key); codes[key]; end
  
  def self.from_lengths(lengths, values = 0 ... lengths.size)
    code = nil
    codes = {}
    lengths.zip(1..lengths.size, values).sort.each do |length, ix, value|
      if length > 0
        raise "code space overrun" if code =~ /^1*$/
        code = code ? code.sub(/01*$/, "1") : "0"
        code = code.ljust(length, "0")
        codes[code] = value
      end
    end
    raise "code space not exhausted" unless code =~ /^1*$/ || lengths.sum < 2
    new codes
  end
  
  def self.from_frequencies(frequencies, values = 0 ... frequencies.size)
    leaves = frequencies.zip(1..frequencies.size).map{|f, i| {f: f, ix: i, leaf: true}}
    inodes = []
    while leaves.size + inodes.size > 1
      left, right = 2.times.map{
        case
        when inodes.empty? then leaves.shift()
        when leaves.empty? then inodes.shift()
        when inodes[0][:f] < leaves[0][:f] then inodes.shift()
        else leaves.shift()
        end
      } 
      inodes << {f: left[:f] + right[:f], left: left, right: right, leaf: false}
    end
    
    collect_leaves = -> node, depth{
      if node[:leaf]
        node[:depth] = depth
        [node]
      else
        collect_leaves[node[:left], depth + 1] + collect_leaves[node[:right], depth + 1]
      end
    }
    node = (leaves + inodes)[0]
    from_lengths collect_leaves[node, 0].sort_by{|leaf| leaf[:ix]}.map{|leaf| leaf[:depth]}
  end
end

################################################################################

def show_parse_header bit_reader
  catch :not_gzip do
    header = bit_reader.peek_bytes(10)
    puts bit_reader.pop_bytes_read_str

    if header[0] != 0x1f || header[1] != 0x8b
      puts "invalid file type ID #{header[0..1].inspect}"
      throw :not_gzip
    end
    puts "0x1f 0x8b - Magic ID"

    if header[2] != 0x08
      puts "unknown compression method #{header[2].inspect}"
      throw :not_gzip
    end
    puts "0x08 - compression method (Deflate)"

    flags = header[3]
    if flags > 31
      puts "reserved flag bit set"
      exit
    end
    
    bit_reader.get_bytes(10)
    puts bit_reader.pop_bytes_read_str

    ftext = flags[0]
    fhcrc = flags[1]
    fextra = flags[2]
    fname = flags[3]
    fcomment = flags[4]

    puts
    puts "flags:"
    puts "#{ftext} - #{"not a " if ftext == 0}text file"
    puts "#{fhcrc} - #{"no " if fhcrc == 0}CRC16 present"
    puts "#{fextra} - #{"no " if fextra == 0}extra fields present"
    puts "#{fname} - #{"no " if fname == 0}file name set"
    puts "#{fcomment} - #{"no " if fcomment == 0}comment present"
    puts

    puts "#{to_bytes_str header[4..7]} - time modified (#{Time.at(header[4..7].bytes_to_int)})"

    puts "#{header[8]} - extra flags (2 = maximum compression, 4 = fastest)"
    puts "#{header[9]} - OS (3 = Unix)"

    if fextra == 1
      length = bit_reader.get_bytes(2).bytes_to_int
      bytes = bit_reader.get_bytes length
      puts bit_reader.pop_bytes_read_str
      puts "extra field: #{bytes}"
    end

    if fname == 1
      str = bit_reader.read_asciiz
      puts bit_reader.pop_bytes_read_str
      puts "filename: #{str}"
    end

    if fcomment == 1
      str = bit_reader.read_asciiz
      puts bit_reader.pop_bytes_read_str
      puts "comment: #{str}"
    end

    if fhcrc == 1
      str =  bit_reader.get_bytes(2).bytes_to_int.to_s(16).rjust(6, "0x0000")
      puts bit_reader.pop_bytes_read_str
      puts "crc: #{str}"
    end
    puts
    return
  end
  
  puts "not gzip; assuming raw deflate"
end

################################################################################

def name_block k
  rnames = %w{R3 R4 R5 R6 R7 R8 R9 R10 R11-12 R13-14
              R15-16 R17-18 R19-22 R23-26 R27-30 R31-34 R35-42 R43-50 R51-58 R59-66
              R67-82 R83-98 R99-114 R115-130 R131-162 R163-194 R195-226 R227-257 R258}
  case k
  when 0 .. 255 then k.chr.bytes_to_glyphs.join
  when 256 then "END"
  when 257..285 then rnames[k-257]
  end
end

def name_offset k
  "O%s" % %w{1 2 3 4 5-6 7-8 9-12 13-16 17-24 25-32
              33-48 49-64 65-96 97-128 129-192 193-256 257-384 385-512 513-768 769-1024
              1025-1536 1537-2048 2049-3072 3073-4096 4097-6144 6145-8192 8193-12288 12289-16384 16385-24576 24577-32768}[k]
end

def name_block_offset((kb, ko)); "#{name_block kb}/#{name_offset ko}"; end

def read_lencodes bit_reader, len_codes, demand
  r = []
  until r.size >= demand
    _, code = bit_reader.read_huffman len_codes
    count = nil
    case code
    when 16 
      val = r.last
      count_bits = bit_reader.get_bits(2)
      count = count_bits.bits_to_int + 3
    when 17 then 
      val = 0
      count_bits = bit_reader.get_bits(3)
      count = count_bits.bits_to_int + 3
    when 18 then
      val = 0 
      count_bits = bit_reader.get_bits(7)
      count = count_bits.bits_to_int + 11
    else 
      val = code
      count_bits = []
      count = 1
    end
    
    r += [val] * count
    print(if count > 1 then "#{count}x #{val}; " else "#{val}; " end)
  end
  raise "too many codes" if r.size > demand
  puts
  r
end

NEW_STR = "\e[32;1mnew \e[0m"
def show_parse_block bit_reader, out_buf, stats, quiet:, extrapolate:
  code_lengths_for = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
  extra_bits = [0, *0..5].map{|c|[c,c,c,c]}.flatten
  extra_offset = [3]
  extra_bits.each {|b| extra_offset << extra_offset.last + 2 ** b}
  extra_bits << 0
  extra_offset[-1] = 258
  oextra_bits = [0, *0..13].map{|c|[c,c]}.flatten
  oextra_offset = [1]
  oextra_bits.each {|b| oextra_offset << oextra_offset.last + 2 ** b}

  puts "#" * 80
  bfinal = bit_reader.get_bits(1) == [1]
  btype = bit_reader.get_bits(2)
  
  puts bit_reader.pop_bytes_read_str
  puts "final: #{bfinal}"
  puts "btype: #{btype}"
  
  if btype == [1, 1]
    puts "invalid block type"
    exit
  end
  
  if btype == [0, 0]
    len0, len1, nlen0, nlen1 = bit_reader.get_bytes(4)
    len = [len0, len1].bytes_to_int
    nlen = ~[nlen0, nlen1].bytes_to_int ^ (-1 << 16)
    puts bit_reader.pop_bytes_read_str
    if len != nlen
      puts "len(#{len}) doesn't match nlen(#{nlen})" unless quiet
      exit
    end
    data = bit_reader.get_bytes(len)
    bit_reader.pop_bytes_read_str
    puts "#{to_bytes_str [len0, len1, nlen0, nlen1]} - #{len} bytes of literal data: " unless quiet
    out_buf.concat *data
    unless quiet
      if len > 25
        puts "#{data[0..9]} ... #{data[-10..-1]}"
      else
        puts data
      end
    end
    return bfinal
  end
  
  litlen_codes = nil
  offset_codes = nil
  if btype == [1, 0]
    puts "block compressed with static Huffman codes: " unless quiet
    litlen_codes = HuffmanCode.from_lengths [*[8]*144, *[9]*112, *[7]*24, *[8]*8]
    offset_codes = HuffmanCode.from_lengths [5]*32
  else # btype == [0, 1]
    puts "block compressed with dynamic Huffman codes: " unless quiet

    hlit = bit_reader.get_bits(5).bits_to_int + 257
    puts bit_reader.pop_bytes_read_str
    puts "#{hlit} literal/length codes" unless quiet
    hdist = bit_reader.get_bits(5).bits_to_int + 1
    puts bit_reader.pop_bytes_read_str
    puts "#{hdist} distance codes" unless quiet
    hclen = bit_reader.get_bits(4).bits_to_int + 4
    puts bit_reader.pop_bytes_read_str
    puts "#{hclen} code length codes" unless quiet

    code_lengths = hclen.times.map{bit_reader.get_bits(3).bits_to_int}
    code_lengths << 0 until code_lengths.size == 19
    code_lengths.sort_by!.with_index{|_, i| code_lengths_for[i]}
    puts bit_reader.pop_bytes_read_str
    puts "code lengths: " + code_lengths.inspect unless quiet

    len_codes = HuffmanCode.from_lengths(code_lengths)
    puts "length codes: " + len_codes.codes.inspect unless quiet

    puts
    lit_lengths = read_lencodes bit_reader, len_codes, hlit
    litlen_codes = HuffmanCode.from_lengths lit_lengths
    puts bit_reader.pop_bytes_read_str
    unless quiet
      puts "Literal + length codes: "
      puts word_wrap digest_hash litlen_codes.codes, value_transform: method(:name_block)
      (1..15).map do |l|
        b = litlen_codes.codes.select{|k, v| k.length == l}.values
        puts "#{l}: #{b.map{|c| name_block c}.join(", ")}".gsub('", "', "") unless b.empty?
      end
    end   
    puts
    
    dist_lengths = read_lencodes bit_reader, len_codes, hdist
    offset_codes = HuffmanCode.from_lengths dist_lengths
    puts bit_reader.pop_bytes_read_str
    unless quiet
      puts "Offset codes: "
      puts word_wrap digest_hash offset_codes.codes, value_transform: method(:name_offset)
    end
  end  

  puts "#" * 80 unless quiet
  fbits = lambda do |str| 
    str.gsub(/\d ?/).with_index do |d, i|
      "#{"\e[0m" if i % 8 == 0}#{"\e[30;1m" if i % 8 == 4}#{d}"
    end + "\e[0m"
  end

  extrapolating = false
  table = Table.new [{}, {nowrap: true}, {}, {}, {}]

  lits_br = []
  lits_bits = []
  push_lits = proc do
    case lits_bits.count
    when 0 then nil
    when 1
      code = out_buf[-1].ord
      table.push_row [
        lits_br[0], 
        "@#{out_buf.size - 1}", 
        "#{fbits[lits_bits[0]]}",
        "#{code.to_s(16).rjust(2, '0')} - #{NEW_STR if stats[:block_counts][code] == 1}" + 
        "literal #{code.chr.bytes_to_glyphs.join}",
        code.chr.bytes_to_glyphs.join
      ]
    else
      str = out_buf.rtake lits_bits.count
      table.push_row [
        lits_br.join(" "),
        "@#{out_buf.size - lits_bits.count}",
        "#{fbits[lits_bits.join " "]}",
        "literal x#{lits_bits.count}",
        str.bytes_to_glyphs.join
      ]
    end
    lits_br = []
    lits_bits = []
  end

  loop do
    at = out_buf.size
    key, code = bit_reader.read_huffman litlen_codes
    stats[:block_counts][code] += 1
    if code < 256
      push_lits[] if stats[:block_counts][code] == 1
      lits_br << bit_reader.pop_bytes_read_str
      lits_bits << key
      out_buf << code.chr
      push_lits[] if stats[:block_counts][code] == 1
      stats[:lit_blocks] += 1
    elsif code == 256
      push_lits[]
      table.push_row [
        bit_reader.pop_bytes_read_str,
        "@#{at}",
        "#{fbits[key]} - #{code}",
        "end of block", 
        ""
      ]
      puts table, "#" * 80 unless quiet
      if bfinal && extrapolate
        bit_reader.start_random!
        extrapolating = true
      else
        return bfinal
      end
    else
      push_lits[]
      extra = bit_reader.get_bits extra_bits[code-257] rescue p [key, code]
      okey, ocode = bit_reader.read_huffman offset_codes
      stats[:offset_counts][ocode] += 1
      stats[:block_offset_counts][[code, ocode]] += 1
      oextra = bit_reader.get_bits oextra_bits[ocode]

      length = extra_offset[code-257] + extra.bits_to_int
      offset = oextra_offset[ocode] + oextra.bits_to_int

      last_buf = String.new # 8-bit ASCII
      buf_start = out_buf.length - offset

      if out_buf.size < offset
        brs = bit_reader.pop_bytes_read_str
        puts table unless quiet
        puts "#{brs} offset = #{offset} but only #{out_buf.size} bytes in output buffer" unless quiet
        exit
      end
      length.times{last_buf << out_buf[-offset]; out_buf << out_buf[-offset]}
      buf_end = out_buf.length - offset
      table.push_row [
        bit_reader.pop_bytes_read_str,
        "@#{at}",
        fbits["#{key} #{extra.join} #{okey} #{oextra.join}"],
        "repeat" +
        " #{NEW_STR if stats[:block_counts][code] == 1}#{length}" +
        " #{NEW_STR if stats[:offset_counts][ocode] == 1}#{offset}",
        "\e[31m#{out_buf[... buf_start].rtake(15).bytes_to_glyphs.rtake(5)&.join}\e[0m" +
        "#{out_buf[buf_start ... buf_end].bytes_to_glyphs.join}" +
        "\e[31m#{out_buf[buf_end ...].take(15).bytes_to_glyphs.take(5)&.join}\e[0m"
      ]
      stats[:rep_blocks] += 1
    end
    puts table.render_last if extrapolating
  end
end

def check_scan(str, scan_to) # hack :-(
  scan_to.nil? || str =~ /^[\h ]*@(\d+)/ && $1.to_i > scan_to.to_i
end

def define_more(scan_to)
  orig_puts = method(:puts)
  fiber = Fiber.new do
    orig_puts[Fiber.yield]
    (str = Fiber.yield; orig_puts[str]) until check_scan(str, scan_to)
    loop do
      case key = $stdin.getch
      when " "
        (IO.console.winsize[0] - 2).times do |i|
          line = Fiber.yield
          p line unless line.is_a? String
          orig_puts[line]
          break if line.include?(NEW_STR) || line =~ /^#*$/
        end
      when "\n", "\r" then orig_puts[Fiber.yield]
      when "\x03", "\x1a" then exit
      else orig_puts["key pressed: #{key.inspect}"]
      end
    end
  end
  define_method :puts do |*args|
    (args.empty? ? [""] : args).each{|arg| [*arg].each{|str| str.to_s.each_line{|line| fiber.resume line}}}
  end
  fiber.resume
    # the first time a fiber is resumed, the arguments go to the block arguments,
    # the rest go to yield returs
end

################################################################################

$Word_wrap_regex = Hash.new{|h, k| h[k] = /(?:(?:\e.*?m)?.(?:\e.*?m)?){1,#{k}}/}
def word_wrap words, width = IO.console.winsize[1] - 1
  words = words.split(" ") if words.is_a? String
  words.reduce [] do |acc, word|
    if word.display_length > width
      word.scan($Word_wrap_regex[width]).each do |word|
        escapes = acc.empty? ? "" : acc.last.scan(/\e.*?m/).join
        escapes.sub /^.*\e\[0m/, ""
        acc.last << "\e[0m" if acc.last
        acc << escapes + word
      end
    elsif acc.empty? || acc.last.display_length + word.display_length + 1 > width
      escapes = acc.empty? ? "" : acc.last.scan(/\e.*?m/).join
      escapes.sub /^.*\e\[0m/, ""
      acc.last << "\e[0m" if acc.last
      acc << escapes + word
    else
      acc.last << " " + word
    end
    acc
  end
end

def list_wrap ary
  word_wrap ary[0 .. -2].map{|e| e + ","} + ary[-1 .. -1]
end

class Table
  def initialize(col_styles, width = IO.console.winsize[1] - 1)
    @col_styles = col_styles
    @col_styles.each.with_index do |style, ix|
      style[:opt_width] = 1
      style[:fixed] = true if style[:width]
    end
    @width = width
    @data = []
  end

  def recalc_widths
    #todo: minimize total height instead of dividing widths equally
    #todo: redistribute rounding errors in the meantime?
    @col_styles.filter{_1[:nowrap]}.each{_1[:width] = _1[:opt_width]}
    fixed_total = @col_styles.filter{_1[:fixed] || _1[:nowrap]}.map{_1[:width]}.sum
    auto_cols = @col_styles.reject{_1[:fixed] || _1[:nowrap]}
    auto_opt_total = auto_cols.map{_1[:opt_width]}.sum
    auto_total = @width - fixed_total - @col_styles.count

    auto_cols.each {_1[:width] = [_1[:opt_width], auto_total].min}
    if auto_total >= auto_opt_total
      return
    end

    auto_cols[0][:width] = auto_total if auto_cols.count == 1
    return if auto_cols.count <= 1

    p auto_cols
    max_key = auto_cols.map{[_1[:opt_width], auto_total].min}
    cell_heights_by_ix_width = Hash.new do |hash, (ix, width)|
      hash[[ix, width]] = @data.map{|row| word_wrap(row[ix], width).count}
    end
    height = lambda do
      @col_styles
        .map.with_index{|s, ix| cell_heights_by_ix_width[[ix, s[:width]]]}
        .transpose.map{_1.max}.sum
    end
    key_fold = (0 ... auto_cols.length).map do |ix|
      Hash.new do |hash, k_i|
        if k_i == 1
          false
        else
          auto_cols.each {_1[:width] = _1[:opt_width]}
          auto_cols[ix][:width] = k_i
          v_i = height[]
          auto_cols[ix][:width] -= 1
          hash[k_i] = v_i == height[]
        end
      end
    end
    (0 ... auto_cols.length).each do |ix|
      max_key[ix] -= 1 while key_fold[ix][max_key[ix]]
    end
    to_expand_by_val = {height[] => [max_key]}
    pending_by_key = {}
    print "starting point: #{max_key.inspect} => #{height[]}\n"
    
    loop do
      old_val = to_expand_by_val.keys.min
      if to_expand_by_val[old_val].empty?
        to_expand_by_val.delete old_val
        redo
      end
      old_key = to_expand_by_val[old_val].pop
      if old_key.sum == auto_total
        auto_cols.zip(old_key).each{_1[:width] = _2}
        print "found widths: #{old_key.inspect} => #{height[]}\n"
        return
      end

      (0 ... old_key.length).each do |ix|
        next if old_key[ix] == 1
        new_key = old_key.dup
        new_key[ix] -= 1
        new_key[ix] -= 1 while key_fold[ix][new_key[ix]]
        pending_by_key[new_key] ||= new_key.zip(max_key).count{_1 != _2}
        pending_by_key[new_key] -= 1
        if pending_by_key[new_key] == 0
          pending_by_key.delete new_key
          auto_cols.zip(new_key).each{_1[:width] = _2}
          new_val ||= height[]
          print "#{new_key.inspect} => #{new_val} \n\e[A" if rand < 0.01
          to_expand_by_val[new_val] ||= []
          to_expand_by_val[new_val] << new_key
        end
      end
    end
  end

  def to_s; render_all; end
  def render_all; recalc_widths; @data.map{render_row _1}.join("\n"); end
  def render_last; render_row @data.last; end

  def height
    @data.map do |row|
      row.zip(@col_styles).map do |cell, style|
        word_wrap(cell, style[:width]).count
      end.max
    end.sum
  end
  def render_row(row)
    cell_rows = row.zip(@col_styles).map{|cell, style| word_wrap cell, style[:width]}
    (0 ... cell_rows.map{_1.count}.max).map do |ix|
      cell_rows.map{_1[ix] || ""}.zip(@col_styles).map do |cell_row, style|
        case style[:just] 
        when :c      then cell_row.center_d style[:width]
        when :l, nil then cell_row.ljust_d style[:width]
        when :r      then cell_row.rjust_d style[:width]
        else raise ArgumentError, "invalid justification"
        end
      end.join(" ")
    end.join("\n")
  end

  def push_row(row)
    if @col_styles.count != row.count
      raise ArgumentError, "expected #{@col_styles.count} columns, found #{row.count} in #{row}"
    end

    row = row.map &:to_s
    @data << row
    row.zip(@col_styles).each do |cell, style|
      style[:opt_width] = [style[:opt_width], cell.display_length].max
    end
  end
end

def digest_hash hash, key_transform: :inspect.to_proc, value_transform: :inspect.to_proc
  gap_size = 0
  prev_key = nil
  r = []
  [*hash.to_a, [nil, nil]].each do |k, v|
    if v == 0
      gap_size += 1
    else
      case gap_size
      when 0 then nil
      when 1 then r << "#{key_transform[prev_key]} => 0"
      else r << "#{gap_size}x 0"
      end
      r << "#{key_transform[k]} => #{value_transform[v]}" if k
      gap_size = 0
    end
    prev_key = k
  end
  r
end

################################################################################


if $0 == __FILE__

  extrapolate = ARGV.include?("--extrapolate")
  ARGV.delete("--extrapolate")
  scan_arg_ix = ARGV.find_index("--scan")
  if scan_arg_ix
    scan_to = ARGV[scan_arg_ix + 1]
    ARGV.slice!(scan_arg_ix, 2)
  end
  if ARGV.include?("--slow")
    ARGV.delete("--slow")
    orig_puts = method(:puts)
    scan_done = false
    define_method(:puts) do |*strs|
      orig_puts.call(*strs)
      scan_done ||= strs.any?{|str| check_scan(str, scan_to)}
      sleep 0.1 if scan_done
    end
  end
  if ARGV.include?("--more")
    ARGV.delete("--more")
    define_more(scan_to)
  end
  
  quiet = ARGV.include?("--headers-only")
  ARGV.delete("--headers-only")
  base64 = ARGV.include?("--base64")
  ARGV.delete("--base64")
  $binary = ARGV.include?("--bin")
  ARGV.delete("--bin")
  
  p_stats = ARGV.include?("--stats")
  ARGV.delete("--stats")
  p_stats2 = ARGV.include?("--stats2")
  ARGV.delete("--stats2")

  hash_stats = %i{block_counts offset_counts block_offset_counts}
  bit_reader = BitReader.new ARGF, base64: base64
  out_buf = String.new encoding:"ASCII-8BIT"
  stats_sum = {
    lit_blocks: 0, rep_blocks: 0, compressed_size: 0, uncompressed_size: 0, 
      block_counts: Hash[(0..285).map{|k| [k,0]}], 
      offset_counts: Hash[(0..29).map{|k| [k,0]}],
      block_offset_counts: Hash[(257..285).flat_map{|kb| (0..29).map{|ko| [[kb, ko], 0]}}]
    }

  show_parse_header bit_reader
  last_cs = 0
  last_ucs = 0
  loop do
    stats = {
      lit_blocks: 0, rep_blocks: 0, 
      block_counts: Hash[(0..285).map{|k| [k,0]}], 
      offset_counts: Hash[(0..29).map{|k| [k,0]}],
      block_offset_counts: Hash[(257..285).flat_map{|kb| (0..29).map{|ko| [[kb, ko], 0]}}]
    }
    last = show_parse_block bit_reader, out_buf, stats, quiet: quiet, extrapolate: extrapolate
    stats[:compressed_size] = ARGF.pos - last_cs; last_cs = ARGF.pos
    stats[:uncompressed_size] = out_buf.size - last_ucs; last_ucs = out_buf.size

    stats_sum.keys.each{|k| stats_sum[k] += stats[k] if stats_sum[k].is_a? Integer}
    hash_stats.each{|k| stats_sum[k].keys.each{|hk| stats_sum[k][hk] += stats[k][hk]}}

    p stats.select{|k, v| v.is_a? Integer}

    if p_stats
      puts list_wrap digest_hash stats[:block_counts], key_transform: method(:name_block)
      puts list_wrap digest_hash stats[:offset_counts], key_transform: method(:name_offset)
      if stats_sum[:compressed_size] > stats[:compressed_size]
        p stats_sum.select{|k, v| v.is_a? Integer}
        puts list_wrap digest_hash stats_sum[:block_counts], key_transform: method(:name_block)
        puts list_wrap digest_hash stats_sum[:offset_counts], key_transform: method(:name_offset)
      end
    end
    if p_stats2
      puts list_wrap digest_hash stats[:block_offset_counts], key_transform: method(:name_block_offset)
      if stats_sum[:compressed_size] > stats[:compressed_size]
        puts list_wrap digest_hash stats_sum[:block_offset_counts], key_transform: method(:name_block_offset)
      end
    end
  break if last
  end
  
  puts "CRC32: " + to_bytes_str(bit_reader.get_bytes(4))
  puts "uncompressed size (reported): " + bit_reader.get_bytes(4).bytes_to_int.to_s
  puts "file size: " + ARGF.pos.to_s
end
