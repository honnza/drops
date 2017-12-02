def day01(input, part)
  input.tr! "^0-9", ""
  pairs = case part
  when :a then (input + input[0]).chars.each_cons(2)
  when :b then input[0 ... input.size/2].chars.zip(input[input.size/2 .. -1].chars) * 2
  end
  pairs.select{|a, b| a==b}.map{|a, b| a.to_i}.reduce(0, &:+)
end

def day02(input, part)
  rows = input.lines.map{|r| r.split(" ").map(&:to_i)}
  case part
  when :a then rows.map{|r| r.max - r.min}.reduce(0, &:+)
  when :b then rows.map{|r| x = nil; r.find{|n| r.find{|d| x = n / d if n != d && n % d == 0}}; x}.reduce(0, &:+)
  end
end

puts "time: #{Time.now}"
p day02(File.read("day02in.txt"), :b)
puts "time: #{Time.now}"
