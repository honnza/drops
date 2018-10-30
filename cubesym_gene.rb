require 'set'
class Permutation
  def initialize(data)
    raise ArgumentError, "expected a permutation, got #{data.inspect}" unless data.sort == (0 ... data.size).to_a
    @data = data
  end
  def self::[](*data); new data; end
  
  attr_reader :data
  
  def *(other)
    return Permutation.new(self * other.data) if other.is_a? Permutation
    return (self * other.chars).join if other.is_a? String
    return (self * other.to_s(2).rjust(@data.size, "0")).to_i(2) if other.is_a? Integer
    
    raise ArgumentError, "length mismatch #{inspect} * #{other.inspect}" unless @data.size == other.size
    @data.map{|i| other[i]}
  end

  include Comparable
  def <=>(other); @data <=> other.data; end
  def eql?(other); @data.eql? other.data; end
  def hash; @data.hash; end
  
  def to_s; "Permutation#{@data}"; end
  def inspect; to_s; end
end

GENERATORS = [Permutation[1, 3, 0, 2, 5, 7, 4, 6], # r01 = t01 + m0
              Permutation[4, 0, 6, 2, 5, 1, 7, 3], # r20 = t20 + m2
              Permutation[2, 3, 6, 7, 0, 1, 4, 5]] # r12 = t12 + m1
INVERSION = Permutation.new((0..7).to_a.reverse)
GENERATORS.dup.each{|g| GENERATORS << g*g << g*g*g << g*g*INVERSION}
PERMUTATIONS = GENERATORS.map{|g1| GENERATORS.map{|g2| [g1 * g2, g1 * g2 * INVERSION]}}.flatten.uniq.sort
CLASSES = (0..255).map{|i| PERMUTATIONS.map{|per| per * i}.uniq.sort}.uniq.sort

################################################################################

def random_genome; CLASSES.map{|c| c.sample}; end
def mix_genomes(x, y); x.zip(y).map(&:sample); end
def mutate(x, p_m); x.map{|g| rand > p_m ? g : GENERATORS.sample * g}; end

def genome_fitness(genome)
  1.0 / genome.map{|gene|
    (0..7).map{|bit|
      next_gene = gene | (1 << bit)
      case
      when genome.include?(next_gene) then 0
      when GENERATORS.any?{|g| genome.include?(g * next_gene)} then 1
      else 2
      end
    }.reduce(&:+)
  }.reduce(&:+)
end

def print_genome(genome)
  genome.map{|g|g.to_s(16).rjust(2, "0")}.join " "
end

population = 16.times.map{random_genome}
best_genome = nil
best_fitness = 0

at_exit do
  puts "#" * 80, "\a1/#{(1/best_fitness).round(5)} #{print_genome best_genome}"
end

2000.times do |generation|
  fitnesses = population.map{|g| genome_fitness g}
  puts generation if generation % 100 == 0
  gen_best = fitnesses.max
  if gen_best > best_fitness
    best_fitness = gen_best
    best_genome = population.find{|g| genome_fitness(g) == best_fitness}
    puts "@#{"%04d"%generation} new best: 1/#{(1/best_fitness).round(5)} for #{print_genome best_genome}"
    #gets
  end
  
  total_fitness = fitnesses.reduce(&:+)
  pop_fitnesses = population.zip(fitnesses)
  population = [best_genome] + (population.size - 1).times.map{
    parents = 2.times.map{
      needle = 0
      stop = rand * total_fitness
      #parent = pop_fitnesses.find{|_, f| needle += f; needle >= stop}.first
      parent = 2.times.map{pop_fitnesses.sample}.max_by{|g, f| [f, rand]}.first
      mutate(parent, 0.05)
    }
    mix_genomes(*parents)
  }
end