require "io/console"

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

def ARGF.read_asciiz
  bytes = []
  bytes << ARGF.read(1) until bytes.last == "\0"
  bytes[0 .. -2].join
end

class BitReader
  def initialize io
    @io = io
    @buffer = []
  end
  
  def get_bits(n)
    while @buffer.size < n
      byte_in = @io.read(1).ord
      (0..7).map{|i| @buffer.push byte_in[i]}
    end
    
    @buffer.shift(n)
  end
  
  def get_bytes(n)
    @buffer = []
    @io.read(n)
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

def show_parse_header
  header = ARGF.read(10).bytes

  if header[0] != 0x1f || header[1] != 0x8b
    puts "invalid file type ID #{header[0..1].inspect}"
    exit
  end
  puts "0x1f 0x8b - Magic ID"

  if header[2] != 0x08
    puts "unknown compression method #{header[2].inspect}"
    exit
  end
  puts "0x08 - compression method (Deflate)"

  flags = header[3]
  if flags > 31
    puts "reserved flag bit set"
    exit
  end

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
    length = ARGF.read(2).bytes.bytes_to_int
    puts "extra field: ", ARGF.read(length)
  end
  puts "filename: ", ARGF.read_asciiz if fname == 1
  puts "comment: ", ARGF.read_asciiz if fcomment == 1
  if fhcrc == 1
    puts "crc: " + ARGF.read(2).bytes.bytes_to_int.to_s(16).rjust(6, "0x0000")
  end
  puts
end

################################################################################

def name_block k
  rnames = %w{R3 R4 R5 R6 R7 R8 R9 R10 R11-12 R13-14
              R15-16 R17-18 R19-22 R23-26 R27-30 R31-34 R35-42 R43-50 R51-58 R59-66
              R67-82 R83-98 R99-114 R115-130 R131-162 R163-194 R195-226 R227-257 R258}
  case k
  when 0 .. 255 then k.chr.inspect
  when 256 then "END"
  when 257..285 then rnames[k-257]
  end
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

def show_parse_block bit_reader, out_buf, stats
  code_lengths_for = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
  extra_bits = [0, *0..5].map{|c|[c,c,c,c]}.flatten
  extra_offset = [3]
  extra_bits.each {|b| extra_offset << extra_offset.last + 2 ** b}
  extra_bits << 0
  extra_offset[-1] = 258
  oextra_bits = [0, *0..13].map{|c|[c,c]}.flatten
  oextra_offset = [1]
  oextra_bits.each {|b| oextra_offset << oextra_offset.last + 2 ** b}

  bfinal = bit_reader.get_bits(1) == [1]
  btype = bit_reader.get_bits(2)
  
  puts "#" * 80
  puts "final: #{bfinal}"
  puts "btype: #{btype}"
  
  if btype == [1, 1]
    puts "invalid block type"
    exit
  end
  
  if btype == [0, 0]
    len0, len1, nlen0, nlen1 = bit_reader.get_bytes(4).bytes
    len = [len0, len1].bytes_to_int
    nlen = ~[nlen0, nlen1].bytes_to_int ^ (-1 << 16)
    if len != nlen
      puts "len(#{len}) doesn't match nlen(#{nlen})"
      exit
    end
    puts "#{to_bytes_str [len0, len1, nlen0, nlen1]} - #{len} bytes of literal data: "
    data = bit_reader.get_bytes(len)
    out_buf << data
    if len > 25
      puts "#{data[0..9]} ... #{data[-10..-1]}"
    else
      puts data
    end
    return bfinal
  end
  
  litlen_codes = nil
  offset_codes = nil
  if btype == [1, 0]
    puts "block compressed with static Huffman codes: "
    litlen_codes = HuffmanCode.from_lengths [*[8]*144, *[9]*112, *[7]*24, *[8]*8]
    offset_codes = HuffmanCode.from_lengths [5]*32
  else # btype == [0, 1]
    puts "block compressed with dynamic Huffman codes: "

    hlit = bit_reader.get_bits(5).bits_to_int + 257
    hdist = bit_reader.get_bits(5).bits_to_int + 1
    hclen = bit_reader.get_bits(4).bits_to_int + 4
    
    puts "#{hlit} literal/length codes"
    puts "#{hdist} distance codes"
    puts "#{hclen} code length codes"
    
    code_lengths = hclen.times.map{bit_reader.get_bits(3).bits_to_int}
    code_lengths << 0 until code_lengths.size == 19
    code_lengths.sort_by!.with_index{|_, i| code_lengths_for[i]}
    puts "code lengths: " + code_lengths.inspect
    
    len_codes = HuffmanCode.from_lengths(code_lengths)
    puts "length codes: " + len_codes.codes.inspect
    
    puts
    lit_lengths = read_lencodes bit_reader, len_codes, hlit
    litlen_codes = HuffmanCode.from_lengths lit_lengths
    puts "Literal + length codes: " + litlen_codes.codes.map{|k, v| "#{k} =>  #{name_block v}"}.join(', ')

    dist_lengths = read_lencodes bit_reader, len_codes, hdist
    offset_codes = HuffmanCode.from_lengths dist_lengths
    puts "Offset codes: " + offset_codes.codes.inspect
  end
  
  (1..15).map do |l|
    b = litlen_codes.codes.select{|k, v| k.length == l}.values
    puts "#{l}: #{b.map{|c| name_block c}.join ", "}" unless b.empty?
  end
  
  puts "#" * 80
  loop do
    key, code = bit_reader.read_huffman litlen_codes
    stats[:block_counts][code] += 1
    if code < 256
      out_buf << code.chr
      print "#{key} - #{code} - #{"new " if stats[:block_counts][code] == 1}literal #{code.chr.inspect[1 .. -2]}".ljust(50)
      puts code.chr.inspect[1 .. -2]
      stats[:lit_blocks] += 1
    elsif code == 256
      puts "#{key} - #{code} - end of block"
      puts "#" * 80
      return bfinal
    else
      extra = bit_reader.get_bits extra_bits[code-257] rescue p [key, code]
      okey, ocode = bit_reader.read_huffman offset_codes
      oextra = bit_reader.get_bits oextra_bits[ocode]
      
      length = extra_offset[code-257] + extra.bits_to_int
      offset = oextra_offset[ocode] + oextra.bits_to_int

      last_buf = String.new # 8-bit ASCII
      buf_start = out_buf.length - offset
      buf_before = [buf_start - 5, 0].max
      length.times{last_buf << out_buf[-offset]; out_buf << out_buf[-offset]}
      buf_end = out_buf.length - offset
      buf_after = [buf_end + 5, out_buf.length].min
      puts "#{key} #{extra.join} #{okey} #{oextra.join} - repeat #{length} #{offset}".ljust(45) +
           "\e[31m#{out_buf[buf_before ... buf_start].join}\e[0m" +
           "#{out_buf[buf_start ... buf_end].join}" +
           "\e[31m#{out_buf[buf_end ... buf_after].join}\e[0m"
      stats[:rep_blocks] += 1
    end
  end
end

def define_more
  orig_puts = method(:puts)
  fiber = Fiber.new do
    IO.console.winsize[0].times{orig_puts[Fiber.yield]}
    loop do
      case key = $stdin.getch
      when " " then (IO.console.winsize[0] - 2).times{orig_puts[Fiber.yield]}
      when "\n" then orig_puts[Fiber.yield]
      else orig_puts "key pressed: #{key}"
      end
    end
  end
  define_method(:puts){|*strs| strs.each{|str| fiber.resume str}}
end

################################################################################

if $0 == __FILE__
  if ARGV.include?("--slow")
    ARGV.delete("--slow")
    orig_puts = method(:puts)
    define_method(:puts){|*strs| orig_puts.call(*strs); sleep 0.1}
  elsif ARGV.include?("--more")
    ARGV.delete("--more")
    define_more
  end
  
  bit_reader = BitReader.new ARGF
  out_buf = []
  stats = {lit_blocks: 0, rep_blocks: 0, block_counts: Hash[(0..285).map{|k| [k,0]}]}

  show_parse_header
  loop do
    last = show_parse_block bit_reader, out_buf, stats
    stats[:compressed_size] = ARGF.pos
    stats[:uncompressed_size] = out_buf.size
    p stats.select{|k, v| v.is_a? Integer}
    break if last
  end
  
  puts "CRC32: " + to_bytes_str(bit_reader.get_bytes(4).bytes)
  puts "uncompressed size (reported): " + bit_reader.get_bytes(4).bytes.bytes_to_int.to_s
  puts "file size: " + ARGF.pos.to_s
  puts stats[:block_counts].select{|k, v| v > 0}.map{|k, v| name_block(k) + " => " + v.inspect}.join ", "
end