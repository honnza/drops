State = Struct.new(:paths, :nodes)

def die str; puts str; exit; end

alphabet = [*"A".."Z", *"a".."z"]
avoids = []
targets = nil

while ARGV.include? "--avoid"
  ix = ARGV.index "--avoid"
  avoids << ARGV[ix + 1]
  2.times{ARGV.delete_at ix}
end

if ARGV.include? "--target"
  ix = ARGV.index "--target"
  targets = [ARGV[ix + 1]]
  2.times{ARGV.delete_at ix}
end

case ARGV.length
when 0
  edgeses = [gets.chomp, gets.chomp].map{|line| line.chars.map{|c|alphabet.index c}}
  avoids.map!{|a| alphabet.index a}
  targets = targets ? targets.map{|a| alphabet.index a} : 0
when 1
  node_labels = Hash[ARGF.take_while{|line| line.chomp !~ /^\#$/}.map{|line| line.chomp.match(/(\S+) (.*)/)[1..2]}]
  node_ids = Hash[node_labels.keys.each_with_index.to_a]
  edgeses = [Array.new(node_ids.size), Array.new(node_ids.size)]
  ARGF.each_line do |line|
    line.chomp =~ /(\S+) (\S+) (.*)/
    die "unknown edge label #{$3.inspect} from #{node_labels[$1]} to #{node_labels[$2]}" unless $3 == "0" || $3 == "1"
    die "multiple #{$3} edges from #{node_labels[$1]}" if edgeses[$3.to_i][node_ids[$1]]
    edgeses[$3.to_i][node_ids[$1]] = node_ids[$2]
  end
  
  
  edgeses.transpose.each.with_index do |edges, ix|
    next if edges.all?
    die "node #{node_labels.values[ix]} missing an outgoing edge" if edges.any?
    candidates = node_labels.select{|k, v| node_ids[k] != ix && v == node_labels.values[ix]}.keys
    die "cannot find match for node #{node_labels.values[ix]} without outedges; candidates = #{candidates.inspect}" if candidates.size != 1
    edgeses.each{|edges| edges.map!{|edge| edge == ix ? node_ids[candidates[0]] : edge}}
  end
  
  avoids = node_labels.select{|k, v| avoids.include? v}.map{|k, v| node_ids[k]}
  targets = targets ? node_labels.select{|k, v| targets.include? v}.map{|k, v| node_ids[k]} : 0
else
  die "don't know what to do with #{ARGV.length} arguments"
end

targets.select!{|ix| edgeses[0][ix]}
puts "avoiding #{avoids.inspect}"
puts "targets are #{targets.inspect}"
queue = [State.new([""], [* 0 ... edgeses[0].count].select{|ix| edgeses[0][ix]})]

states_by_nodes = {queue[0].nodes => queue[0]}
prev_path_length = -1
final_state = catch :done do
  queue.each do |state|
    throw :done, state if state.nodes.all?{|n| targets.include? n}
    edgeses.each.with_index do |edges, tix|
      new_nodes = state.nodes.map{|n| edges[n]}.sort.uniq
      new_paths = state.paths.map{|path| path + tix.to_s}
      .reject{|path|!avoids.empty? && avoids.include?(path.chars.reduce(targets[0]){|a, c| edgeses[c.to_i][a]})}
      next if new_paths.empty?
      ex_state = states_by_nodes[new_nodes]
      if ex_state
        ex_state.paths += new_paths if ex_state.paths[0].length == new_paths[0].length
      else
        queue << (states_by_nodes[new_nodes] = State.new(new_paths, new_nodes))
      end
      puts "#{queue.length} states opened" if prev_path_length < new_paths[0].length
      prev_path_length = new_paths[0].length
    end
  end
  nil
end

if final_state
  puts final_state.paths.sort
else
  puts "sync word not found"
end
print ?\a