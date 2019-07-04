def overlay_bitmap(bitmap, mask, over_char, offset)
  (0 ... mask.size).each do |i|
    (0 ... mask[i].size).each do |j|
      unless mask[i][j].nil?
        bitmap[i + offset[0]][j + offset[1]] = over_char || mask[i][j]
      end
    end
  end
end

TETROMINO_STR = <<END
 I    J  L       S    T   Z 
 I    J  L   OO  SS  TT  ZZ 
 I   JJ  LL  OO   S   T  Z  
 I                          
END
TETROMINO_SHAPES = TETROMINO_STR.lines.map{|l|l.each_slice(4)}.transpose.map do |s|
  s.delete("    ")
  s.map!{|l| l.chars.map{|c| c == " " ? nil : c}}
  s = s.transpose
  s.delete{|l| l.compact.empty?}
  s
end
def make_tetromino(render_char = nil)
  r = TETROMINO_SHAPES.sample
  rand(4).times{r = r.transpose.reverse}
  r.each{|l| r.map!{|c| render_char if c}} if(render_char)
  r
end
  
################################################################################

GameState = Struct.new :board_bitmap, :faller_shape, :faller_pos, :score do
  def valid_state?
  (0 .. faller_shape.size).all? do |i|
    (0 .. faller_shape[i].size).all? do |j|
      board_i = i + faller_pos[0]
      board_j = j + faller_pos[1]
      faller_shape[i][j].nil? || 
          (0 ... board_bitmap.size).include?(board_i) &&
          (0 ... board_bitmap[board_i]).include?(board_j) &&
          board_bitmap[board_i][board_j].nil?
    end
  end
  
  def render_board(static_char = nil, faller_char = nil, ghost_char = nil)
    r = board_bitmap.map{|row| row.map{|cell| cell.nil? ? " " : (static_char || cell)}}
    ghost_di = 0
    ghost_di += 1 while valid_move?([di + 1, 0])
    overlay_bitmap!(r, faller_shape, ghost_char, [faller_pos[0] + ghost_di, faller_pos[1]]) unless ghost_char == " "
    overlay_bitmap!(r, faller_shape, faller_char, faller_pos) unless faller_char == " "
  end
  
  def valid_rotation?(dir); rotate_faller(dir).valid_state?; end
  def rotate_faller(dir)
    new_faller = case dir
    when :cw then faller_shape.reverse.transpose
    when :ccw then faller_shape.transpose.reverse
    when :180 then faller_shape.reverse.map(&:reverse)
    end
    shift = dir == :180 ? 0 : (faller_shape.size - faller_shape[0].size) / 2
    dup.tap{|r| r.faller_shape = new_faller; faller_pos[0] -= shift; faller_pos[1] += shift}
  end
  
  def move_faller((i, j)); dup.tap{|r| r.faller_pos = r.faller_pos.dup.tap{|fp| fp[0] += i; fp[1] += j}}; end
  def valid_move?(dir); move_faller(dir).valid_state?; end
  
  def freeze_faller
    (0 .. faller_shape.size).all? do |i|
      (0 .. faller_shape[i].size).all? do |j|
        board_i = i + faller_pos[0]
        board_j = j + faller_pos[1]
          board_bitmap[board_i][board_j] ||= faller_shape[i][j]
      end
    end
  end
end

