# frozen_string_literal: true

require 'json'

$heaven_mode = ARGV.include? "-h"
$chaos_mode = ARGV.include? "-c"

#todo: move to a separate file
default_cfg = [
  #tier 0 lands
  {type: "land", code: "ICYL", prereqs: [], name: "Icy Land"},
  {type: "land", code: "LICA", prereqs: [], name: "Living Cave"},
  {type: "land", code: "DESE", prereqs: [], name: "Desert"},
  {type: "land", code: "HUNT", prereqs: [], name: "Hunting Ground"},
  {type: "land", code: "LAEE", prereqs: [], name: "Land of Eternal Motion"},
  {type: "land", code: "JUNL", prereqs: [], name: "Jungle"},
  {type: "land", code: "ALHM", prereqs: [], name: "Alchemist Lab"},

  #tier 1 lands
  {type: "land", code: "HALO", prereqs: [30],         name: "Hall of Mirrors"},
  {type: "land", code: "MINF", prereqs: [30],         name: "Minefield"},
  {type: "land", code: "PACE", prereqs: [30],         name: "Palace"},
  {type: "land", code: "ZEBR", prereqs: [30, "LAEE"], name: "Zebra"},
  {type: "land", code: "JELY", prereqs: [30, "ALHM"], name: "Jelly Kingdom"},
  {type: "land", code: "REPI", prereqs: [30, "ALHM"], name: "Reptiles"},
  #coastal/aquatic lands
  {type: "land", code: "OCEN", prereqs: [30],     name: "Ocean"},
  {type: "land", code: "WRPE", prereqs: [30],     name: "Warped Coast"},
  {type: "land", code: "LIFJ", prereqs: [30],     name: "Living Fjord"},
  {type: "land", code: "KRAK", prereqs: ["LIFJ"], name: "Kraken Depths"},
  {type: "land", code: "CARI", prereqs: [30],     name: "Caribbean"},
  {type: "land", code: "BROW", prereqs: [30],     name: "Brown Island"},
  {type: "land", code: "WHIR", prereqs: [30],     name: "Whirlpool"},
  {type: "land", code: "RLYE", prereqs: [60],     name: "R'Lyeh"},
  {type: "land", code: "TEMP", prereqs: ["RLYE"], name: "Temple of Cthulhu"}, #only needs 5 treasure
  #gravity lands
  #FRFA / DUNG / LOSM only requre five treasure of each, but this script doesn't model that
  {type: "land", code: "IVOR", prereqs: [30],             name: "Ivory Tower"},
  {type: "land", code: "YEIA", prereqs: ["IVOR"],         name: "Yendorian Forest"},
  {type: "land", code: "FRFA", prereqs: ["IVOR", "LAEE"], name: "Free Fall"},
  {type: "land", code: "DUNG", prereqs: ["IVOR", "PACE"], name: "Dungeon"},
  {type: "land", code: "LOSM", prereqs: ["IVOR", "JUNL"], name: "Lost Mountain"},

  #tier 2 goals
  {type: "pasv", code: "100K", prereqs: [],       name: "100 kills"},
  {type: "pasv", code: "20DK", prereqs: [],       name: "20 different kills"},
  {type: "goal", code: "SKEL", prereqs: ["PACE"], name: "kill Skeleton"},
  {type: "goal", code: "VIZI", prereqs: ["PACE"], name: "kill Vizier"},
  #tier 2 lands
  {type: "land", code: "DRYF", prereqs: [60],             name: "Dry Forest"},
  {type: "land", code: "VINY", prereqs: [60],             name: "Vineyard"},
  {type: "land", code: "DEAD", prereqs: [60, "LICA"],     name: "Dead Cave"},
  {type: "land", code: "GRAV", prereqs: ["100K"],         name: "Graveyard"},
  {type: "land", code: "HAUE", prereqs: ["GRAV"],         name: "Haunted Woods"},
  {type: "land", code: "HIVE", prereqs: [60, "100K"],     name: "Hive"},
  {type: "land", code: "RERO", prereqs: [60, "DESE"],     name: "Red Rock Valley"},
  {type: "land", code: "VOLC", prereqs: [60, "ALHM"],     name: "Volcanic Wasteland"},
  {type: "land", code: "DRAG", prereqs: ["20DK"],         name: "Dragon Chasms"},
  {type: "land", code: "GALA", prereqs: ["20DK"],         name: "Galapagos"},
  {type: "land", code: "OVER", prereqs: [60, "JUNL"],     name: "Overgrown Woods"},
  {type: "land", code: "CLER", prereqs: ["OVER"],         name: "Clearing"}, #only needs 5 treasure
  {type: "land", code: "LAST", prereqs: [60],             name: "Land of Storms"},
  {type: "land", code: "BURI", prereqs: ["KRAK"],         name: "Burial Grounds"},
  {type: "land", code: "WIND", prereqs: [60],             name: "Windy Plains"},
  {type: "land", code: "BLIZ", prereqs: ["WIND", "ICYL"], name: "Blizzard"},
  {type: "land", code: "RUIN", prereqs: ["SKEL"],         name: "Ruined City"},
  {type: "land", code: "EMER", prereqs: [[["DRYF", "LICA"], ["VIZI"]]], name: "Emerald Mine"},
  {type: "land", code: "IRRD", prereqs: ["RUIN", "EMER", "GRAV"],       name: "Irradiated Field"},

  #Tier 3 lands
  {type: "land", code: "PRAI", prereqs: [90], name: "Prairie"},
  {type: "land", code: "BULL", prereqs: [90], name: "Bull Dash"},
  {type: "land", code: "TRRA", prereqs: [90], name: "Terracotta Army"},
  {type: "land", code: "ROSE", prereqs: [60], name: "Rose Garden"},
  #Elementals
  {type: "goal", code: "AIRL", prereqs: ["WIND"], name: "kill Air Elemental"},
  {type: "goal", code: "WATE", prereqs: ["LIFJ"], name: "kill Water Elemental"},
  {type: "goal", code: "EART", prereqs: ["DEAD"], name: "kill Earth Elemental"},
  {type: "goal", code: "FIEE", prereqs: ["DRAG"], name: "kill Fire Elemental"},
  {type: "land", code: "ELEM", prereqs: %w{AIRL WATE EART FIEE}, name: "Elemental Planes"},
  #Trolls
  {type: "goal", code: "ROCK", prereqs: [],           name: "kill Rock Troll"},
  {type: "goal", code: "DARK", prereqs: [60, "LICA"], name: "kill Dark Troll"},
  {type: "goal", code: "RETL", prereqs: [60, "DESE"], name: "kill Red Troll"},
  {type: "goal", code: "STOR", prereqs: [60],         name: "kill Storm Troll"},
  {type: "goal", code: "FORS", prereqs: [60, "JUNL"], name: "kill Forest Troll"},
  {type: "goal", code: "FJOD", prereqs: [30],         name: "kill Fjord Troll"},
  {type: "land", code: "TROL", prereqs: %w{ROCK DARK RETL STOR FORS FJOD}, name: "Trollheim"},
  #Hell
  {type: "land", code: "HELL", prereqs: [90],             name: "Hell"}, #requires 9x10 treasure
  {type: "land", code: "COCY", prereqs: ["HELL", "ICYL"], name: "Cocytus"},
  {type: "land", code: "LAPW", prereqs: ["HELL"],         name: "Land of Power"},
  #Main quests
  #PAQU, CAME do generate treasure, but they aren't required for HYPE.
  #Counting them as goels matters exactly twice, and even then only a little.
  {type: "goal", code: "PAQU", prereqs: ["VIZI"],         name: "Palace Quest"},
  {type: "goal", code: "CAME", prereqs: ["EMER", "GRAV"], name: "Camelot Quest"}, #listed as "Camelot" in-game
  {type: "goal", code: "YEQU", prereqs: ["HELL", "GRAV"], name: "Yendor Quest"},
  {type: "goal", code: "HYPE",                            name: "Hyperstone Quest"},
]

if $chaos_mode
  default_cfg.reject! do |land| 
    puts "gotta unlock chaos mode first!"
    quit
    %w{}.include? land[:code]
  end
  default_cfg.find{|land| land[:code] == "MM"}[:prereqs] = ["DYF", "VV"]
end

default_cfg.last[:prereqs] = default_cfg.select{|land| land[:type] == "land"}.map{|land| land[:code]}

class Land
  def initialize(type:, code:, prereqs:, name:)
    @type = type
    @code = code
    @prereqs = prereqs
    @name = name
    @survivals = 0.0
    @deaths = 0.0
  end
  attr_reader :code, :name, :type
  attr_accessor :survivals, :deaths, :done

  def land?; @type == "land"; end
  def passive?; @type == "pasv"; end

  def priority
    if $heaven_mode
      [Rational(@survivals + 1, @deaths + @survivals + 1)]
    else
      [Rational(@deaths + 1, @deaths + @survivals + 1)]
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
    when Numeric then $lands_done >= prereq / 10
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
  $lands.reject(&:passive?).each do |land|
    puts "%22s: %f deaths, %f survivals, %f %s" %
      [land.name, land.deaths, land.survivals, land.priority.last, $heaven_mode ? "survival rate" : "death rate"]
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
      if land.passive?
        puts "#{land} is awaiting completion"
      elsif land.unlocked?
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
    when /survived (.+)/i
      land = $lands.find{|land| land.name.casecmp($1) == 0 || land.code.casecmp($1) == 0}
      land.survivals += 1
      $lands_done += 1 if land.land?
      land.done = true
    when /skip (.+)/i
      land = $lands.find{|land| land.name.casecmp($1) == 0 || land.code.casecmp($1) == 0}
      $lands_done += 1 if land.land?
      land.done = true
    when /died in (.+)/i
      land = $lands.find{|land| land.name.casecmp($1) == 0 || land.code.casecmp($1) == 0}
      land.survivals -= 1 if land.done
      land.deaths += 1
      restart
      p_stats
    when /died en-route/i
      restart
      p_stats
    when /quit/i
      break
    when /help/i
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
