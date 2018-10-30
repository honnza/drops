# frozen_string_literal: true

require 'json'

$heaven_mode = ARGV.include? "-h"
$chaos_mode = ARGV.include? "-c"

#todo: move to a separate file
default_cfg = [

  {land: true, code: "CYL", prereqs: [], name: "Icy Land"},
  {land: true, code: "VV",  prereqs: [], name: "Living Cave"},
  {land: true, code: "DSE", prereqs: [], name: "Desert"},
  {land: true, code: "HG",  prereqs: [], name: "Hunting Ground"},
  {land: true, code: "FN",  prereqs: [], name: "Land of Eternal Motion"},
  {land: true, code: "JU",  prereqs: [], name: "Jungle"},
  {land: true, code: "LB",  prereqs: [], name: "Alchemist Lab"},

  {land: true,  code: "MIR", prereqs: [3],       name: "Mirror Land"},
  {land: true,  code: "MIF", prereqs: [3],       name: "Minefield"},
  {land: true,  code: "ZE",  prereqs: [3, "FN"], name: "Zebra"},
  {land: true,  code: "JY",  prereqs: [3, "LB"], name: "Jelly Kingdom"},
  {land: true,  code: "PAL", prereqs: [3],       name: "Palace"},
  {land: false, code: "Q",   prereqs: ["vk"],    name: "Palace Quest"},
  {land: true,  code: "OCN", prereqs: [3],       name: "Ocean"},
  {land: true,  code: "LJ",  prereqs: [3],       name: "Living Fjord"},
  {land: true,  code: "WC",  prereqs: [3],       name: "Warped Coast"},

  #The water-based worlds are only accessible through the ocean,
  #but the ocean treasure is only present on the beach.
  #The in-game help is vague here, but the official guide on steam says r'lyeh unlocks at $60
  {land: true, code: "RB",  prereqs: [3],    name: "Carribean"},
  {land: true, code: "WH",  prereqs: [3],    name: "Whirlpool"},
  {land: true, code: "YH",  prereqs: [6],    name: "R'Lyeh"},
  {land: true, code: "TF",  prereqs: ["YH"], name: "Temple of Cthulhu"},

  #technically, 100k can be reached with zero treasure, but it's not fun.
  #20dk is possible, if unlikely, to reach with just the 30-treasure lands.
  {land: false, code: "100k", prereqs: [[]],    name: "100 kills"},
  {land: false, code: "20dk", prereqs: [[]],    name: "20 different kills"},
  {land: false, code: "vk",   prereqs: ["PAL"], name: "kill a vizier"},

  {land: true, code: "DYF", prereqs: [6],         name: "Dry Forest"},
  {land: true, code: "VIY", prereqs: [6],         name: "Vineyard"},
  {land: true, code: "DED", prereqs: [6, "VV"],   name: "Dead Cave"},
  {land: true, code: "GY",  prereqs: ["100k"],    name: "Graveyard"},
  {land: true, code: "HW",  prereqs: ["GY"],      name: "Haunted Woods"},
  {land: true, code: "HV",  prereqs: [6, "100k"], name: "Hive"},
  {land: true, code: "DK",  prereqs: [6, "DSE"],  name: "Red Rock Valley"},
  {land: true, code: "CW",  prereqs: [6, "LB"],   name: "Volcanic Wasteland"},
  {land: true, code: "YW",  prereqs: [3],         name: "Ivory Tower"},
  {land: true, code: "YEN", prereqs: ["YW"],      name: "Yendorian Forest"},
  {land: true, code: "GH",  prereqs: ["20dk"],    name: "Dragon Chasms"},
  {land: true, code: "GP",  prereqs: ["20dk"],    name: "Galapagos"},

  {land: true, code: "GW",  prereqs: [6, 'JU'],     name: "Overgrown Woods"},
  {land: true, code: "CG",  prereqs: ["GW"],        name: "Clearing"},
  {land: true, code: "LFS", prereqs: [6],           name: "Land of Storms"},
  {land: true, code: "WY",  prereqs: [6],           name: "Windy Plains"},
  {land: true, code: "BZ",  prereqs: ["CYL", "WY"], name: "Blizzard"},
  {land: true, code: "UC",  prereqs: ["PAL"],       name: "Ruined City"},
  {land: true, code: "MY",  prereqs: [9],           name: "Terracotta Army"},
  {land: true, code: "SG",  prereqs: [9],           name: "Rose Garden"},

  {land: true,  code: "MM",   prereqs: [[["DYF", "VV"], ["vk"]]], name: "Emerald Mine"},
  {land: false, code: "CAO",  prereqs: ["MM", "GY"],              name: "Camelot"},
  {land: true,  code: "ELP",  prereqs: ["GH", "DED", "LJ", "WY"], name: "Elemental Planes"},
  {land: true,  code: "HELL", prereqs: [9],                       name: "Hell"},
  {land: true,  code: "YU",   prereqs: ["HELL", "CYL"],           name: "Cocytus"},
  {land: true,  code: "OP",   prereqs: ["HELL"],                  name: "Land of Power"},
  
  #version 9.4 lands
  
  {land: true, code: "RPI", prereqs: [3, "LB"],                              name: "Reptiles"},
  {land: true, code: "KR",  prereqs: ["LJ"],                                 name: "Kraken Depths"},
  {land: true, code: "DU",  prereqs: ["YW", "PAL"],                          name: "Dungeon"},
  {land: true, code: "SU",  prereqs: ["YW", "JU"],                           name: "Lost Mountain"},
  {land: true, code: "BG",  prereqs: ["KR"],                                 name: "Burial Grounds"},
  {land: true, code: "PRA", prereqs: [9],                                    name: "Prairie"},
  {land: true, code: "BH",  prereqs: [9],                                    name: "Bull Dash"},
  {land: true, code: "TRH", prereqs: ["VV", "LJ", "DED", "LFS", "GW", "DK"], name: "Trollheim"}
  
]

if $chaos_mode
  default_cfg.reject! do |land| 
    %w{vk Q RB WH HW YW YEN CG CAO DU SU PRA}.include? land[:code]
  end
  default_cfg.find{|land| land[:code] == "MM"}[:prereqs] = ["DYF", "VV"]
end

class Land
  def initialize(land:, code:, prereqs:, name:)
    @land = land
    @code = code
    @prereqs = prereqs
    @name = name
    @survivals = 0.0
    @deaths = 0.0
  end
  attr_reader :code, :name, :land
  attr_accessor :survivals, :deaths, :done

  def priority
    if $heaven_mode
      Rational(@survivals + 1, @deaths + @survivals + 1)
    else
      Rational(@deaths + 1, @deaths + @survivals + 1)
    end
  end

  def save_data; {code: @code, survivals: @survivals, deaths: @deaths}; end

  # a land is unlocked when all of its prerequisites have been satisfied
  def unlocked?; prereqs_sat @prereqs; end
  # check if a list of prerequisites has been satisfied
  private def prereqs_sat prereqs; prereqs.all? {|p| prereq_sat p}; end
  # check if a single prerequisite has been satisfied
  private def prereq_sat prereq
    case prereq
    when Numeric then $lands_done >= prereq
    when String then $lands.find{|l| l.code == prereq}.done rescue raise ArgumentError, "unknown land or goal #{prereq}"
    when Array then prereq.any?{|q| prereqs_sat q}
    else raise ArgumentError, "unknown prerequisite type #{prereq.Class.name}"
    end
  end

  # generate a list of lands that contribute to unlocking this land (and haven't been completed yet)
  # empty iff this land is unlocked.

  # if there is a specific prerequisite, its land always contributes unless satisfied
  # if there is only a numeric prerequisite, no lands are suggested
  # if there is a multiple-choice prerequisite, its options' contributing lands are unified
  # The interaction between multiple-choice prerequisites and numeric prerequisites
  #   is yet to be specified
  def suggest_prereqs
    pres = @prereqs.flatten
    spec_pres = pres.select{|pre| pre.is_a? String}
                    .map{|pre| $lands.find{|land| land.code == pre}}
                    .reject(&:done)

    # sort specific prerequisites using the global land priority
    $lands & spec_pres
  end
  
  def to_s; "#{name}(#{code})#{" (undefeated)" if survivals == 0}"; end
end

def p_stats
  $lands.select(&:land).each do |land|
    puts "%22s: %f deaths, %f survivals, %f %s" %
      [land.name, land.deaths, land.survivals, land.priority, $heaven_mode ? "survival rate" : "death rate"]
  end
end

def plist(list, pre_sg="", pre_pl="", post_sg="", post_pl="")
  list = list.map &:to_s
  case list.size
  when 0 then "[]"
  when 1 then "#{pre_sg}#{list[0]}#{post_sg}"
  else "#{pre_pl}#{list[0..-2].join(", ")} and #{list[-1]}#{post_pl}"
  end
end

$lands = default_cfg.map{|land_data| Land.new land_data}

begin
  open "hrtfh#{".chaos" if $chaos_mode}.log" do |file|
    JSON.parse(file.read).map do |rec|
      land = $lands.find{|land| land.code == rec["code"]}
      next unless land
      land.deaths = rec["deaths"]
      land.survivals = rec["survivals"]
    end
  end
rescue Errno::ENOENT
  puts "hrfth#{".chaos" if $chaos_mode}.log not found; starting anew", ""
end

$lands = [$lands[0]] + $lands[1..-1].sort_by{|land|[land.priority, rand]}
$lands_done = 0

def restart
  $lands.each{|land| land.done = false; land.survivals *= 0.9; land.deaths *= 0.9}
  $lands = [$lands[0]] + $lands[1..-1].sort_by{|land|[land.priority, rand]}
  $lands_done = 0
end

p_stats
puts "", "welcome to HyperRogue trainer from #{$heaven_mode ? "heaven" : "hell"}#{", chaos mode" if $chaos_mode}." + 
     " We'll try to kill you as #{$heaven_mode ? "late" : "soon"} as possible. Type 'help' for the list of commands.", ""
loop do
  if !$chaos_mode && !$lands.first.done
    puts "You start in the #{$lands.first}. Type \"survived #{$lands.first.code}\" after collecting ten treasure."
  elsif $lands.all? &:done
    puts "You have completed the goals in every land except the crossroads. All that is left to do is to hoard the hyperstones."
  else
    puts $lands.reject(&:done).map(&:code).join(" ")
    stack = [{for: nil, prereqs: $lands.reject(&:done)}]
    loop do
      stack.pop while stack.last[:prereqs].empty?
      land = stack.last[:prereqs].pop
      if land.unlocked?
        puts "#{land} is unlocked."
        break
      else
        prereqs = land.suggest_prereqs
        puts "#{stack.map{|h|h[:for]&.code}.join(" > ")} > #{land.code} > #{plist prereqs.map &:code}"
        stack << {for: land, prereqs: prereqs}
      end
    end

    puts "", "How did you fare?"
  end


  begin
    case $stdin.gets.chomp
    when /survived (.+)/
      land = $lands.find{|land| land.name == $1 || land.code == $1}
      land.survivals += 1
      $lands_done += 1 if land.land
      land.done = true
    when /skip (.+)/
      land = $lands.find{|land| land.name == $1 || land.code == $1}
      $lands_done += 1 if land.land
      land.done = true
    when /died in (.+)/
      land = $lands.find{|land| land.name == $1 || land.code == $1}
      land.survivals -= 1 if land.done
      land.deaths += 1
      restart
      p_stats
    when /died en-route/
      restart
      p_stats
    when /quit/
      break
    when /help/
      puts "I'm sorry, the help section is under construction"
    else
      puts "I didn't understand"
    end
  rescue
    puts $!, ""
  end

  puts ?- * 80
  sleep 0.5
end

open "hrtfh#{".chaos" if $chaos_mode}.log", "w" do |file|
  file.puts JSON.generate $lands.map(&:save_data)
end
