require "io/console"
require "json"
require "zlib"
require "set"

class WordPredictor
  def initialize(goods = [], bads = [])
	@samples_by_letters = Hash.new{|h, k| h[k] = {good: Set.new, bad: Set.new}}
  @diphtong_stats = Hash.new(|h, k| h[k] = {good: 0, bad: 0})
	goods.each{|w| add_good w}
	bads.each{|w| add_bad w}
  end
  
	def add_good(sample); add(sample, :good); end
	def add_bad(sample); add(sample, :bad); end
	def add(sample, status)
		samples = samples_by_letters[sample.chars.sort]
		if samples[status].add? sample
			sample.chars.each_cons(2) do |d|
				diphtong_stats[d][status] += 1
			end
		end
	end
	
	def test(sample)
		samples_by_letters[sample.chars.sort].each do |status, samples|
			return status == :good ? 1.0 : 0.0 if samples.include? sample
		end
		sample.chars.each_cons(2).map do |a|
			stats = diphtong_stats[a]
			(stats[:good] + 1) / (stats[:good] + stats[:bad] + 2)
		end.reduce(&:*, 1)
	end
	
	def serialize
		[
			@samples_by_letters.values.map{|x| x[:good]}.flatten,  
			@samples_by_letters.values.map{|x| x[:bad]}.flatten
		]
	end
	def self.deserialize(x); self.new(*x); end
end

class WordGrid
	
end