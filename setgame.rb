DECK = %w{rgb hsf stc}.reduce [""] do |as, ds|
  as.flat_map{|a| ds.chars.map{|d| a+d}}
end

def solve(cards)
  cards.combination(3).select do |c3|
    c3.map(&:chars).transpose.all?{|p3|p3.uniq.size.odd?}
  end
end

def gen_cases(n)
  n.times.map do
    cards = DECK.shuffle.take(9)
    solution = solve cards
    p [cards, solution.size, solution]
  end
end