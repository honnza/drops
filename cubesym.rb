VOWELS = %w{a e i o u}
CONSONANTS = [* "a".."z"] - VOWELS

implications = ARGV.include?("-i")

def oct2bin(oct); oct.to_i(8).to_s(2).rjust(27, "0"); end
def bin2oct(bin); bin.to_i(2).to_s(8).rjust( 9, "0"); end
def permute(bin, permutation); permutation.map{|i|bin[i]}.join; end
def permute_oct(oct, permutation); bin2oct(permute(oct2bin(oct), permutation)); end
def bin2words(oct);
  oct.scan(/((....).(....))/).map{|m|
    CONSONANTS[m[1].to_i(2)] + VOWELS[m[0].to_i(2) % 5] + CONSONANTS[m[2].to_i(2)]
  }.join(" ")
end

generators = [[ 0,  3,  6,   1,  4,  7,   2,  5,  8,
                9, 12, 15,  10, 13, 16,  11, 14, 17,  #mirror YZ
               18, 21, 24,  19, 22, 25,  20, 23, 26],
              
              [ 0,  1,  2,   9, 10, 11,  18, 19, 20,
                3,  4,  5,  12, 13, 14,  21, 22, 23,  #mirror XY
                6,  7,  8,  15, 16, 17,  24, 25, 26],
                
              [ 2,  1,  0,   5,  4,  3,   8,  7,  6,
               11, 10,  9,  14, 13, 12,  17, 16, 15,  #mirror Z
               20, 19, 18,  23, 22, 21,  26, 25, 24]]
               
bit_groups = [[4, 10, 12, 14, 16, 22],
              [* 1.step(25, 2)] - [13],
              [0, 2, 6, 8, 18, 20, 24, 26]]

###############################################################################

first_code = $stdin.gets
first_code = oct2bin(first_code) unless first_code.length >= 27
codes = nil

begin
  puts
  codes = [first_code]
  codes.each do |code|
    if implications && code[0] == "0" && code[4] == "1" && code[9] == "1"
      first_code = code
      first_code[0] = "1"
      raise nil
    end
    if implications && code[1] == "0" && code[4] == "1" && code[10] == "1"
      first_code = code
      first_code[1] = "1"
      raise nil
    end
    generators.each do |gen|
      new_code = permute(code, gen)
      puts("#{bin2words(code)} => #{bin2words(new_code)}")
      codes << new_code unless codes.include? new_code
    end
  end
rescue
  retry
end

print "#{codes.first.count("1")}/"
bit_groups.each{|group| print group.count{|bit| codes.first[bit] == "1"}}
puts
codes.sort_by{|code| [code[4], code[22], code]}.map do |code|
  puts("#{code} #{bin2oct(code)} #{bin2words(code)}")
end