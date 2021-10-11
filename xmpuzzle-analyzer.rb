require 'zlib'
require 'nokogiri'

def block_place_piece(piece_voxels, placement)
    dx, dy, dz, rot = placement.split.map &:to_i

    r = Hash.new{"_"}
    piece_voxels.each.with_index do |slice, z|
        slice.each.with_index do |row, y|
            row.each.with_index do |voxel, x|
                tx, ty, tz = [
                    [x,  y,  z], [x, -z,  y], [x, -y, -z], [x,  z, -y]
                ][rot % 4]
                tx, ty, tz = [
                    [ tx,  ty,  tz], [-tz,  ty,  tx], 
                    [-tx,  ty, -tz], [ tz,  ty, -tx], 
                    [-ty,  tx,  tz], [ ty, -tx,  tz]
                ][rot / 4]

                # tx, ty, tz = [
                #     [ x,  y,  z], [ x, -z,  y], [ x, -y, -z], [ x,  z, -y],
                #     [-z,  y,  x], [-y, -z,  x], [ z, -y,  x], [ y,  z,  x],
                #     [-x,  y, -z], [-x, -z, -y], [-x, -y,  z], [-x,  z,  y],
                #     [ z,  y, -x], [ y, -z, -x], [-z, -y, -x], [-y,  z, -x],
                #     [-y,  x,  z], [ z,  x,  y], [ y,  x, -z], [-z,  x, -y],
                #     [ y, -x,  z], [-z, -x,  y], [-y, -x, -z], [ z, -x, -y]
                # ][rot]
                r[[tz + dz, ty + dy, tx + dx]] = voxel if voxel != "_"
            end
        end
    end
    r
end

def block_render(piece_voxels, goal_voxels, placement)

    sx = goal_voxels[0][0].size
    sy = goal_voxels[0].size
    sz = goal_voxels.size

    yx = (0 ... sy).to_a.reverse.map do |y|
        (0 ... sx).map do |x|
            case
            when (0 ... sz).any? {|z| piece_voxels[[z, y, x]] != "_"} then "# "
            when (0 ... sz).any? {|z| goal_voxels[z][y][x] != "_"} then ". "
            else "  "
            end
        end
    end
    zx = (0 ... sz).to_a.reverse.map do |z|
        (0 ... sx).map do |x|
            case
            when (0 ... sy).any? {|y| piece_voxels[[z, y, x]] != "_"} then "# "
            when (0 ... sy).any? {|y| goal_voxels[z][y][x] != "_"} then ". "
            else "  "
            end
        end
    end
    zy = (0 ... sz).to_a.reverse.map do |z|
        (0 ... sy).to_a.reverse.map do |y|
            case
            when (0 ... sx).any? {|x| piece_voxels[[z, y, x]] != "_"} then "# "
            when (0 ... sx).any? {|x| goal_voxels[z][y][x] != "_"} then ". "
            else "  "
            end
        end
    end

    # +X = right | +y = back (up within layer) | +z = up (up across layers)
    # We one of three projections based on the shortest dimension:
    #   -> x | z UU   |   UU z
    #        |        |
    # ^ UU R | ^ FF R | F RR ^
    # | UU R | | FF R | F RR |
    #        |        |
    # y FF   |   -> x | y ->

    case
    when sz == 1 then yx.map &:join
    when sy == 1 then zx.map &:join
    when sx == 1 then zy.map &:join
    when sx < sy && sx < sz
        # x goes down or right; -y goes left; -z goes down
        xx = [" " * sx] * sx
        
        xx.zip(yx.transpose).map{|xxr, xyr| xxr + " " + xyr.reverse.join}.join("\n") +
        "\n\n" +
        zx.zip(zy).map{|zxr, zyr| zxr.join + " " + zyr.join}.join("\n")
    when sy < sz
        # x goes right; -y goes down or left; -z goes down
        yy = [" " * sy] * sy

        yx.zip(yy).map{|yxr, yyr| yxr .join+ " " + yyr}.join("\n") +
        "\n\n" +
        zx.zip(zy).map{|zxr, zyr| zxr.join + " " + zyr.reverse.join}.join("\n")
    else
        zz = [" " * sz] * sz

        # x goes right; -y goes down; -z goes down or right
        yx.zip(zy.transpose).map{|yxr, yzr| yxr.join + " " + yzr.join}.join("\n") + 
        "\n\n" +
        zx.zip(zz).map{|zxr, zzr| zxr.join + " " + zzr}.join("\n")
    end
end

def sort_pieces(pieces)
    rank_mag = 1
    rank_list = [0, 1, 2, 3, 5, 7, 10]
    while pieces.map{|_, v| v[:count]}.sum > rank_mag * 10
        rank_list << rank_mag * 14 << rank_mag * 20 << rank_mag * 30 <<
                     rank_mag * 45 << rank_mag * 67 << rank_mag * 100
        rank_mag *= 10
    end
    rank = -> x {rank_list.find{x <= _1}}
    p rank_list

    voxel_score = Hash.new{|h, k| h[k] = Hash.new{0}}
    candidates = pieces.values
    voxel_choices = candidates.flat_map{|piece| piece[:voxels].keys}.tally
    candidates.each {|piece| piece[:n_ties] = 0}
    until candidates.empty?
        plan_candidates = candidates.dup
        plan_score = voxel_score.dup.transform_values! &:dup
        plan_last = nil
        candidates.each do |piece| 
            piece[:score] = piece[:voxels].keys
                                .map(&voxel_score[piece[:key].first]).sort
        end

        piece = loop do
            plan_candidates.each do |piece|
                piece[:plan_score] = piece[:voxels].keys
                                        .map(&plan_score[piece[:key].first]).sort
                piece[:batch] = [
                    rank[piece[:plan_score].first], 
                    piece[:key].first,
                    piece[:plan_score].first,
                    -piece[:n_ties]
                ]
            end
            piece = plan_candidates.min_by do |v|
                [
                    v[:batch], 
                    v[:count] ,# * v[:score].count{rank[v[:score].first] == rank[_1]}, 
                    v[:plan_score]
                ]
            end
            break plan_last if !piece || plan_last && plan_last[:batch] != piece[:batch]
            plan_last = piece
            piece[:voxels].keys.each{|k| plan_score[piece[:key].first][k] += piece[:count]}
            plan_candidates.delete_if{_1[:key] == piece[:key] || _1[:batch] != piece[:batch]}
            p [
                piece[:n_ties], piece[:name], piece[:plan_score], piece[:count]
            ]
        end
        
        candidates.delete_if{_1[:key] == piece[:key]}
        candidates.each do
            if  _1[:key].first == piece[:key].first && 
                _1[:score] == piece[:score] && 
                _1[:count] == piece[:count]
            then
                _1[:n_ties] += 1
                p _1[:name]
            end
        end
        piece[:voxels].keys.each{|k| voxel_score[piece[:key].first][k] += piece[:count]}
        yield piece
    end
end

grid_types = {
    "0" => {
        name: "block",
        place_piece: method(:block_place_piece),
        render: method(:block_render)
    }
}

more_mode = ARGV.include?("--more")
ARGV.delete("--more")
show_choices = ARGV.include?("--choices") || ARGV.include?("--placements")
ARGV.delete("--choices")
ARGV.delete("--placements")
show_cells = ARGV.include?("--cells")
ARGV.delete("--cells")
show_solutions = ARGV.include?("--solutions")
ARGV.delete("--solutions")


if ARGV.length != 1
    abort <<~END
        usage: filename [--more] [--placements|--choices] [--cells] [--solutions]
    END
end

xml = Nokogiri::XML.parse Zlib::GzipReader.open(ARGV[0]){_1.read}

nodes = xml.css("> puzzle")
if nodes.count != 1
    abort " 1 puzzle node expected, #{nodes.count} found"
end
if nodes[0][:version] != "2"
    abort "version 2 file expected, #{nodes[0][:version]} found"
end

nodes = xml.css("> puzzle > gridType") 
if nodes.count != 1
    abort "1 grid type node expected, #{nodes.count} found"
end
grid_type = grid_types[nodes[0][:type]]
if grid_type
    puts "grid type #{grid_type[:name]}"
else
    puts "unknown grid type #{nodes[0][:type]}"
end

# <colors> <color red green blue /> ...

nodes = xml.css("> puzzle > shapes") # not to be confused with problem > shapes
if nodes.count != 1
    abort "1 shape list node expected, #{nodes.count} found"
end
shape_names = nodes.css("> voxel").map{|n| n[:name]}
voxels = nodes.css("> voxel").map do |n|
    x = n[:x]&.to_i
    y = n[:y]&.to_i
    z = n[:z]&.to_i
    if !x || !y || !z || n[:type] != "0"
        abort "could not parse #{n} as a puzzle shape"
    end
    if n.text !~ /^(_|[#+]\d*)*$/
        abort "could not parse #{n.text} as puzzle shape voxel data"
    end
    voxel_bits = n.text.scan /[_+#]\d*/
    abort "unexpected number of voxel bits in #{n}" if voxel_bits.count != x * y * z
    voxel_bits.each_slice(x).to_a.each_slice(y).to_a
end
puts "#{shape_names.count} shapes found"

nodes = xml.css("> puzzle > problems")
if nodes.count != 1
    abort "1 problem list node expected, #{nodes.count} found"
end
nodes[0].css("> problem").each.with_index(1) do |n_problem, i_problem|
    partial = false
    print "problem P#{i_problem}#{" - " + n_problem[:name] if n_problem[:name]}: "
    case n_problem[:state]
    when "0"
        puts "no progress"
        next
    when "1"
        puts "calculation in progress"
        partial = true
    when "2"
        puts "done"
    else
        abort "unknown problem state #{n_problem[:state].inspect}"
    end

    nodes = n_problem.css("> shapes") # 0-indexes into puzzle > shapes
    if nodes.count != 1
        abort "exactly 1 problem shape list node expected, #{nodes.count} found"
    end
    problem_shape_data = nodes.css("> shape").flat_map.with_index do |n, i|
        id = n[:id]&.to_i

        count = n[:count]&.to_i
        min = n[:min]&.to_i
        max = n[:max]&.to_i

        if !id || !count && !(min && max) || count && (min || max)
            abort "could not parse #{n} as a problem shape"
        end
        min = max = count if count
        (1 .. max).map do |j|
            [i, id, [
                "S", (i+1).to_s, 
                ("." + j.to_s if max > 1), 
                (" - " + shape_names[id] if shape_names[id])
            ].join]
        end
    end

    nodes = n_problem.css("> result")
    if nodes.count != 1
        abort "1 result node expected, #{nodes.count} found"
    end
    goal_id = nodes[0]["id"]&.to_i
    abort "could not parse #{nodes[0]["id"]} as node ID" unless goal_id

    # <bitmap> <pair piece result /> ... - permitted color assignments

    nodes = n_problem.css("> solutions")
    next if nodes.count == 0
    if nodes.count > 1
        abort "at most 1 solution list node expected, #{nodes.count} found"
    end
    solution_nodes = nodes[0].css("> solution")
    if solution_nodes.count == n_problem[:assemblies].to_i
        puts "#{solution_nodes.count} solutions"
    else
        puts "\e[31msome solutions have not been stored " +
            "(#{n_problem[:assemblies]} expected, #{solution_nodes.count} found)\e[0m"
        partial = true
        sleep 0.5
    end
    pieces = {}
    solutions = solution_nodes.map.with_index(1) do |n, assembly_id|
        nodes = n.css("> assembly")
        if nodes.count != 1
            abort "1 assembly node expected, #{nodes.count} found"
        end
        
        # negative placement offsets can occur if the piece wasn't trimmed
        if nodes[0].text !~ /^(-?\d+ -?\d+ -?\d+ \d+ |x )*(-?\d+ -?\d+ -?\d+ \d+|x)$/
            abort "unknown format for assembly #{nodes[0].text.strip}"
        end
        assembly_bits = nodes[0].text.scan /-?\d+ -?\d+ -?\d+ \d+|x/
        if assembly_bits.count != problem_shape_data.count
            abort "unexpected assembly length for assembly #{nodes[0].text.strip}"
        end
        
        {
            id: assembly_id,
            pieces: assembly_bits.zip(problem_shape_data).map do |placement, (pid, gid, name)|
                pieces[[pid, placement]] ||= {
                    key: [pid, placement],
                    name: "##{assembly_id}/#{name}",
                    voxels: (grid_type[:place_piece][voxels[gid], placement] if grid_type),
                    count: 0
                }
                pieces[[pid, placement]][:count] += 1

                pieces[[pid, placement]]
            end
        }
    end

    if show_choices
        if grid_type
            sort_pieces(pieces) do |piece|
                puts "#{piece[:name]} (#{piece[:key]}) x#{piece[:count]}"
                puts piece[:score].inspect
                puts grid_type[:render][piece[:voxels], voxels[goal_id], piece[:key].last]
                STDIN.gets if more_mode
            end
        else
            pieces.to_a.sort_by{|k, v| [k.first, v[:count]]}.each do |k, v|
                puts "#{v[:name]} (#{k}) x#{v[:count]}"
                STDIN.gets if more_mode
            end
        end
    end

    if show_cells
        sums = pieces.values
                        .flat_map{|v| v[:voxels].keys.map{|k| [k, v[:count]]}}
                        .group_by(&:first)
                        .transform_values{|kvs| kvs.map(&:last).sum}

        w = sums.values.max.to_s.length
        sums.keys.map{_1[0]}.max.downto sums.keys.map{_1[0]}.min do |z|
            unless sums.keys.any?{_1[0] == z}
                puts "(blank slice)"
                next
            end

            sums.keys.map{_1[1]}.max.downto sums.keys.map{_1[1]}.min do |y|
                sums.keys.map{_1[2]}.min.upto sums.keys.map{_1[2]}.max do |x|
                    print "\e[44m%*d \e[0m" % [w, sums[[z, y, x]]] rescue print " " * (w+1)
                end
                puts
            end
            puts
            STDIN.gets if more_mode
        end
    end

    if show_solutions
        sums = pieces.values
                        .flat_map{|v| v[:voxels].keys.map{|k|
                            [[k, v[:key].first], v[:count]]
                        }}.group_by(&:first)
                        .transform_values{|kvs| kvs.map(&:last).sum}

        solutions.each do |solution|
            solution[:score] = solution[:pieces].flat_map do |v|
                v[:voxels].keys.map{|k| sums[[k, v[:key].first]]}
            end.sort
        end

        solutions.sort_by{_1[:score]}.each do |solution|
            puts "#{solution[:id]}: #{solution[:score]}"
            STDIN.gets if more_mode
        end
    end
end