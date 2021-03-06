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
end

class String
  def display_length; gsub(/\e.*?m/,"").length; end
  def ljust_d(len); self + " " * [len - display_length, 0].max; end
  def bytes_to_glyphs
    r = ""
    prev_high = false
    self.each_byte do |b|
      if b <= 127
        r += "\e[27m" if prev_high
        prev_high = false
        c = b
      else
        r += "\e[7m" if !prev_high
        prev_high = true
        c = 255 - b
      end
      r += case c when 0x00 .. 0x1F then (0x2400 + c).chr(Encoding::UTF_8)
                  when 0x20         then "\u2423"
                  when 0x21 .. 0x7E then c.chr(Encoding::UTF_8)
                  when 0x7F         then "\u2421"
                  end
    end
    r + (prev_high ? "\e[27m" : "")
  end
end

class BitReader
  def initialize io, base64:
    @io = io
    @bits_read = 0
    @buffer = []
    @base64 = base64
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
  
  def p_byte b; print "\e[30;1m%02X\e[0m " % b; b; end
  def p_str s; print "\e[30;1m%02s\e[0m " % s; s; end
  
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
    puts

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

    ftext = flags[0]
    fhcrc = flags[1]
    fextra = flags[2]
    fname = flags[3]
    fcomment = flags[4]

    puts
    puts "flags:"
    puts "#{ftext} - text file?"
    puts "#{fhcrc} - CRC16 present?"
    puts "#{fextra} - extra fields present?"
    puts "#{fname} - file name set?"
    puts "#{fcomment} - comment present?"
    puts

    puts "#{to_bytes_str header[4..7]} - time modified (#{Time.at(header[4..7].bytes_to_int)})"

    puts "#{header[8]} - extra flags (2 = maximum compression, 4 = fastest)"
    puts "#{header[9]} - OS (3 = Unix)"

    if fextra == 1
      length = bit_reader.get_bytes(2).bytes_to_int
      puts "extra field: ", bit_reader.get_bytes(length)
    end
    puts "filename: ", bit_reader.read_asciiz if fname == 1
    puts "comment: ", bit_reader.read_asciiz if fcomment == 1
    if fhcrc == 1
      puts "crc: " + bit_reader.get_bytes(2).bytes_to_int.to_s(16).rjust(6, "0x0000")
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
  when 0 .. 255 then k.chr.bytes_to_glyphs
  when 256 then "END"
  when 257..285 then rnames[k-257]
  end
end

def name_offset k
  "O%s" % %w{1 2 3 4 5-6 7-8 9-12 13-16 17-24 25-32
              33-48 49-64 65-96 97-128 129-192 193-256 257-384 385-512 513-768 769-1024
              1025-1536 1537-2048 2049-3072 3073-4096 4097-6144 6145-8192 8193-12288 12289-16384 16385-24576 24577-32768}[k]
end

def read_lencodes bit_reader, len_codes, demand
  r = []
  until r.size >= demand
    key, code = bit_reader.read_huffman len_codes
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
    if len != nlen
      puts "len(#{len}) doesn't match nlen(#{nlen})" unless quiet
      exit
    end
    puts "#{to_bytes_str [len0, len1, nlen0, nlen1]} - #{len} bytes of literal data: " unless quiet
    data = bit_reader.get_bytes(len)
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
    hdist = bit_reader.get_bits(5).bits_to_int + 1
    hclen = bit_reader.get_bits(4).bits_to_int + 4

    unless quiet
      puts "#{hlit} literal/length codes"
      puts "#{hdist} distance codes"
      puts "#{hclen} code length codes"
    end

    code_lengths = hclen.times.map{bit_reader.get_bits(3).bits_to_int}
    code_lengths << 0 until code_lengths.size == 19
    code_lengths.sort_by!.with_index{|_, i| code_lengths_for[i]}
    puts "code lengths: " + code_lengths.inspect unless quiet

    len_codes = HuffmanCode.from_lengths(code_lengths)
    puts "length codes: " + len_codes.codes.inspect unless quiet

    puts
    lit_lengths = read_lencodes bit_reader, len_codes, hlit
    litlen_codes = HuffmanCode.from_lengths lit_lengths
    unless quiet
      puts "Literal + length codes: " unless quiet
      puts word_wrap digest_hash litlen_codes.codes, value_transform: method(:name_block)
      (1..15).map do |l|
        b = litlen_codes.codes.select{|k, v| k.length == l}.values
        puts "#{l}: #{b.map{|c| name_block c}.join(", ")}".gsub('", "', "") unless b.empty?
      end
    end   
    puts
    
    dist_lengths = read_lencodes bit_reader, len_codes, hdist
    offset_codes = HuffmanCode.from_lengths dist_lengths
    unless quiet
      puts "Offset codes: "
      puts word_wrap digest_hash offset_codes.codes, value_transform: method(:name_offset)
    end
  end  

  puts "#" * 80 unless quiet
  loop do
    at = out_buf.size
    key, code = bit_reader.read_huffman litlen_codes
    stats[:block_counts][code] += 1
    if code < 256
      out_buf << code.chr
      puts "@#{at} #{key} - #{code} - #{NEW_STR if stats[:block_counts][code] == 1}"\
            "literal #{code.chr.bytes_to_glyphs}".ljust_d(50) + code.chr.bytes_to_glyphs unless quiet
      stats[:lit_blocks] += 1
    elsif code == 256
      puts "@#{at} #{key} - #{code} - end of block" unless quiet
      puts "#" * 80
      if bfinal && extrapolate
        bit_reader.start_random!
      else
        return bfinal
      end
    else
      extra = bit_reader.get_bits extra_bits[code-257] rescue p [key, code]
      okey, ocode = bit_reader.read_huffman offset_codes
      stats[:offset_counts][ocode] += 1
      oextra = bit_reader.get_bits oextra_bits[ocode]

      length = extra_offset[code-257] + extra.bits_to_int
      offset = oextra_offset[ocode] + oextra.bits_to_int

      last_buf = String.new # 8-bit ASCII
      buf_start = out_buf.length - offset
      buf_before = [buf_start - 5, 0].max
      if out_buf.size < offset
        puts "offset = #{offset} but only #{out_buf.size} bytes in output buffer" unless quiet
        exit
      end
      length.times{last_buf << out_buf[-offset]; out_buf << out_buf[-offset]}
      buf_end = out_buf.length - offset
      buf_after = [buf_end + 5, out_buf.length].min
      unless quiet
        puts ("@#{at} #{key} #{extra.join} #{okey} #{oextra.join} - repeat" +
              " #{NEW_STR if stats[:block_counts][code] == 1}#{length}" +
              " #{NEW_STR if stats[:offset_counts][ocode] == 1}#{offset}").ljust_d(45) +
              "\e[31m#{out_buf[buf_before ... buf_start].bytes_to_glyphs}\e[0m" +
              "#{out_buf[buf_start ... buf_end].bytes_to_glyphs}" +
              "\e[31m#{out_buf[buf_end ... buf_after].bytes_to_glyphs}\e[0m"
      end
      stats[:rep_blocks] += 1
    end
  end
end

def check_scan(str, scan_to) # hack :-(
  scan_to.nil? || str =~ /^@(\d+)/ && $1.to_i > scan_to.to_i
end

def define_more(scan_to)
  orig_puts = method(:puts)
  fiber = Fiber.new do
    (str = Fiber.yield; orig_puts[str]) until check_scan(str, scan_to)
    loop do
      case key = $stdin.getch
      when " "
        (IO.console.winsize[0] - 2).times do |i|
          line = Fiber.yield
          p line unless line.is_a? String
          orig_puts[line]
          break if line.include? NEW_STR
        end
      when "\n", "\r" then orig_puts[Fiber.yield]
      when "\x03" then exit
      else orig_puts["key pressed: #{key.inspect}"]
      end
    end
  end
  define_method(:puts){|*args| args.each{|arg| [*arg].each{|str| fiber.resume str}}}
end

################################################################################

def word_wrap words, width = IO.console.winsize[1] - 1
  words[1 .. -1].reduce [words[0]] do |acc, word|
    if acc.last.length + word.length >= width
      acc << word
    else
      acc.last.concat " " + word
    end
    acc
  end
end

def list_wrap ary
  word_wrap ary[0 .. -2].map{|e| e + ","} + ary[-1 .. -1]
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
  
  hash_stats = %i{block_counts offset_counts}
  bit_reader = BitReader.new ARGF, base64: base64
  out_buf = String.new encoding:"ASCII-8BIT"
  stats_sum = {lit_blocks: 0, rep_blocks: 0, compressed_size: 0, uncompressed_size: 0, 
                block_counts: Hash[(0..285).map{|k| [k,0]}], offset_counts: Hash[(0..29).map{|k| [k,0]}]}

  show_parse_header bit_reader
  last_cs = 0
  last_ucs = 0
  loop do
    stats = {lit_blocks: 0, rep_blocks: 0, 
              block_counts: Hash[(0..285).map{|k| [k,0]}], offset_counts: Hash[(0..29).map{|k| [k,0]}]}
    last = show_parse_block bit_reader, out_buf, stats, quiet: quiet, extrapolate: extrapolate
    stats[:compressed_size] = ARGF.pos - last_cs; last_cs = ARGF.pos
    stats[:uncompressed_size] = out_buf.size - last_ucs; last_ucs = out_buf.size

    stats_sum.keys.each{|k| stats_sum[k] += stats[k] if stats_sum[k].is_a? Integer}
    hash_stats.each{|k| stats_sum[k].keys.each{|hk| stats_sum[k][hk] += stats[k][hk]}}

    p stats.select{|k, v| v.is_a? Integer}
    
    puts list_wrap digest_hash stats[:block_counts], key_transform: method(:name_block)
    puts list_wrap digest_hash stats[:offset_counts], key_transform: method(:name_offset)
    if stats_sum[:compressed_size] > stats[:compressed_size]
      p stats_sum.select{|k, v| v.is_a? Integer}
      puts list_wrap digest_hash stats_sum[:block_counts], key_transform: method(:name_block)
      puts list_wrap digest_hash stats_sum[:offset_counts], key_transform: method(:name_offset)
    end
    break if last
  end
  
  puts "CRC32: " + to_bytes_str(bit_reader.get_bytes(4))
  puts "uncompressed size (reported): " + bit_reader.get_bytes(4).bytes_to_int.to_s
  puts "file size: " + ARGF.pos.to_s
end