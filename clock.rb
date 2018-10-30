require 'zlib'
require 'base64'
font = Zlib.inflate(Base64.decode64(DATA.read)).split("\n").each_slice(10).to_a

class IO
  def has_data?
    #source: http://stackoverflow.com/a/948077/
    result = IO.select([self], nil, nil, 0)
    result && (result.first.first == self)
  end
end

puts "\e[2J\e[H"
until $stdin.has_data?
  puts "\e[11A"
  time_text = Time.now.strftime "%HA%MA%S"
  glyphs = time_text.chars.map{|c| font[c.to_i(16)]}
  glyphs.transpose.map{|line| puts "     " + line.join("  ")}
  sleep 0.1
end

__END__
eJyNUcsNQyEMu2cKS720hyoLIWWRDN/8oKX0FSIhK3xsxwAiAhAYQAMpvPQY
0ew9gwhZhqxIwkDjzX6DRqO2nGh2xF3pEzke+hVKCOE0wCKnRGE92oax365H
i+FSUUtRS8DQ/dsQeV+xIElWhYc3Sjn9cnLmaM7g6DcHUXc0K/KfkH//3hL2
leLufEu0EK4RoByNjCTxfntk/+zZHZ57vQBQZoyW