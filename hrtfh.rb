# frozen_string_literal: true

require 'json'

$heaven_mode = ARGV.include? "-h"
$chaos_mode = ARGV.include? "-c"
$filename = if ARGV.include?("-f") 
            then ARGV[ARGV.find_index("-f") + 1] 
            else "hrtfh#{".chaos" if $chaos_mode}.log"
            end
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
  {type: "land", code: "FROG", prereqs: ["REPI", "ZEBR", "JELY"], name: "Frog Park"}, #any 24 of the three types
  {type: "land", code: "WETA", prereqs: [30],     name: "Wetlands"},
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
  {type: "kill", code: "SKEL", prereqs: ["PACE"], name: "kill Skeleton"},
  {type: "kill", code: "VIZI", prereqs: ["PACE"], name: "kill Vizier"},
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
  {type: "land", code: "DICE", prereqs: [90],             name: "Dice Reserve"},
  {type: "land", code: "OVER", prereqs: [60, "JUNL"],     name: "Overgrown Woods"},
  {type: "land", code: "CLEA", prereqs: ["OVER"],         name: "Clearing"}, #only needs 5 treasure
  {type: "land", code: "LAST", prereqs: [60],             name: "Land of Storms"},
  {type: "land", code: "BURI", prereqs: ["KRAK"],         name: "Burial Grounds"},
  {type: "land", code: "WIND", prereqs: [60],             name: "Windy Plains"},
  {type: "land", code: "BLIZ", prereqs: ["WIND", "ICYL"], name: "Blizzard"},
  {type: "land", code: "RUIN", prereqs: ["SKEL"],         name: "Ruined City"},
  {type: "land", code: "EMER", prereqs: [[["DRYF", "LICA"], ["VIZI"]]],   name: "Emerald Mine"},
  {type: "land", code: "IRRD", prereqs: ["RUIN", "EMER", "GRAV"],         name: "Irradiated Field"},
  {type: "land", code: "ECLC", prereqs: ["ICYL", "LAST", "PACE", "DEAD"], name: "Eclectic City"},
  # Cursed Canyon can be unlocked by any four of ALHM (0), CARI, BROW (30), RUIN (30 + PACE), LAPW (90)
  # Ideally the algorithm would prioritize preqs that can be achieved the soonest. For now, we'll help it.
  {type: "land", code: "CURE", prereqs: ["ALHM", "CARI", "RUIN", "BROW"], name: "Cursed Canyon"},

  #Tier 3 lands
  {type: "land", code: "PRAI", prereqs: [90], name: "Prairie"},
  {type: "land", code: "BULL", prereqs: [90], name: "Bull Dash"},
  {type: "land", code: "TRRA", prereqs: [90], name: "Terracotta Army"},
  {type: "land", code: "ROSE", prereqs: [90], name: "Rose Garden"},
  #Elementals
  {type: "kill", code: "AIRL", prereqs: ["WIND"], name: "kill Air Elemental"},
  {type: "kill", code: "WATE", prereqs: ["LIFJ"], name: "kill Water Elemental"},
  {type: "kill", code: "EART", prereqs: ["DEAD"], name: "kill Earth Elemental"},
  {type: "kill", code: "FIEE", prereqs: ["DRAG"], name: "kill Fire Elemental"},
  {type: "land", code: "ELEM", prereqs: %w{AIRL WATE EART FIEE}, name: "Elemental Planes"},
  #Trolls
  {type: "kill", code: "ROCK", prereqs: [],           name: "kill Rock Troll"},
  {type: "kill", code: "DARK", prereqs: [60, "LICA"], name: "kill Dark Troll"},
  {type: "kill", code: "RETL", prereqs: [60, "DESE"], name: "kill Red Troll"},
  {type: "kill", code: "STOR", prereqs: [60],         name: "kill Storm Troll"},
  {type: "kill", code: "FORS", prereqs: [60, "JUNL"], name: "kill Forest Troll"},
  {type: "kill", code: "FJOD", prereqs: [30],         name: "kill Fjord Troll"},
  {type: "land", code: "TROL", prereqs: %w{ROCK DARK RETL STOR FORS FJOD}, name: "Trollheim"},
  #Hell
  {type: "land", code: "HELL", prereqs: [90],             name: "Hell"}, #requires 9x10 treasure
  {type: "land", code: "COCY", prereqs: ["HELL", "ICYL"], name: "Cocytus"},
  {type: "land", code: "LAPW", prereqs: ["HELL"],         name: "Land of Power"},
  #Main quests
  #PAQU, CAME do generate treasure, but they aren't required for HYPE.
  #Counting them as goals matters exactly twice, and even then only a little.
  {type: "goal", code: "PAQU", prereqs: ["VIZI"],         name: "Palace Quest"},
  {type: "goal", code: "CAME", prereqs: ["EMER", "GRAV"], name: "Camelot Quest"}, #listed as "Camelot" in-game
  {type: "goal", code: "YEQU", prereqs: ["HELL", "GRAV"], name: "Yendor Quest"},
  {type: "goal", code: "HYPE",                            name: "Hyperstone Quest"},
]

if $chaos_mode
  default_cfg.reject! do |land| 
    %w{PAQU CARI BROW WHIR IVOR YEIA FRFA DUNG LOSM HAUE CLEA CAME}.include? land[:code]
  end
end

default_cfg.last[:prereqs] = default_cfg.select{|land| land[:type] == "land"}.map{|land| land[:code]}

# class Array; include Comparable; end

class Land
  def initialize(type:, code:, prereqs:, name:)
    @type = type
    @code = code
    @prereqs = prereqs
    @name = name
    @survivals = 0.0
    @deaths = 0.0
    @state = "todo"
  end
  attr_reader :code, :name, :type, :prereqs
  attr_accessor :survivals, :deaths, :state

  def land?; @type == "land"; end
  def passive?; @type == "pasv" || (@type == "kill" && $chaos_mode) || @state == "skip"; end
  def done?; @state == "done"; end
  def colored_code; "\e[1;#{unlocked? ? 37 : 30}m#{code}\e[0m"; end

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
    when Numeric then $lands_done >= prereq / 10
    when String then $lands.find{|l| l.code == prereq}.done?
    when Array then prereq.any?{|q| prereqs_sat q}
    else raise ArgumentError, "unknown prerequisite type #{prereq.Class.name} of #{code}"
    end
  rescue
    raise ArgumentError, "unknown prerequisite #{prereq.inspect} of #{code}"
  end

  # generate a list of lands that contribute to unlocking this land (and haven't been completed yet)
  # empty iff this land is unlocked.

  # if there is a specific prerequisite, its land always contributes unless satisfied
  # if there is only a numeric prerequisite, no lands are suggested
  # if there is a multiple-choice prerequisite, its options' contributing lands are unified
  # The interaction between multiple-choice prerequisites and numeric prerequisites
  #   is yet to be specified
  def suggest_prereqs; suggest_with_score(prereqs).last; end

  private def suggest_with_score pres
    score = []
    r = []
    pres.each do |pre|
      case pre
      when Numeric
        score << pre / 10
      when String 
        ix = $lands.find_index{|land| land.code == pre}
        unless $lands[ix].done?
          score << ix
          r << $lands[ix]
        end
      when Array
        opt_score, opt_r = pre.map{|opt| suggest_with_score opt}.min
        score << opt_score
        r |= opt_r
      end
    end
    [score.sort, r]
  end
  
  def to_s; "#{name}(#{code})#{" \e[36m(undefeated)\e[0m" if survivals == 0}"; end
end

def p_stats
  $lands.reject(&:passive?).each do |land|
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

$lands = default_cfg.map{|land_data| Land.new **land_data}

begin
  open $filename do |file|
    JSON.parse(file.read).map do |rec|
      land = $lands.find{|land| land.code == rec["code"]}
      next unless land
      land.deaths = rec["deaths"]
      land.survivals = rec["survivals"]
    end
  end
rescue Errno::ENOENT
  puts "#{$filename} not found; starting anew", ""
end

def restart
  $lands.each do |land|
    land.survivals += 1 if land.done?
    land.state = "todo"
    land.survivals *= 0.9
    land.deaths *= 0.9
  end
  $lands_done = 0
  sort_lands
end

def sort_lands
  $lands.sort_by!.with_index do |land, ix|
    [(!$chaos_mode && ix == 0) ? 0 : 1, -land.priority, rand]
  end
  loop do
    puts $lands.map(&:colored_code).join(" ")
    new_lands = $lands.sort_by.with_index do |land, ix_land|
      ix_l = $lands.find_index{|l| l == land || l.suggest_prereqs.include?(land)}
      [ix_l, ix_l == ix_land ? 1 : 0, ix_land]
    end
    break if $lands == new_lands
    $lands = new_lands
  end
end

$lands_done = 0
sort_lands
p_stats
puts  "", "welcome to HyperRogue trainer from #{$heaven_mode ? "heaven" : "hell"}#{", chaos mode" if $chaos_mode}." + 
      " We'll try to kill you as #{$heaven_mode ? "late" : "soon"} as possible. Type 'help' for the list of commands.", ""
loop do
  $lands_done = $lands.count{|land| land.land? && land.done?}

  if !$chaos_mode && !$lands.first.done?
    puts "You start in the #{$lands.first}. Type \"survived #{$lands.first.code}\" after collecting ten treasure."
  elsif $lands.all? &:done?
    puts "You have completed the goals in every land except the crossroads. All that is left to do is to hoard the hyperstones."
  else
    puts $lands.reject(&:done?).map(&:colored_code).join(" ")
    stack = [{for: nil, prereqs: $lands.reject(&:done?).reverse}]
    loop do
      stack.pop while stack.last[:prereqs].empty?
      land = stack.last[:prereqs].pop
      if land.unlocked?
        if land.passive?
          puts "\e[32m#{land} \e[32mis awaiting completion\e[0m"
        else
          puts "#{land} is unlocked."
          break
        end
      else
        prereqs = $lands & land.suggest_prereqs
        puts "#{stack.map{|h|h[:for]&.code}.join(" > ")} > #{land.code} > #{plist prereqs.map &:code}"
        stack << {for: land, prereqs: prereqs}
      end
    end

    puts "", "How did you fare?"
  end


  begin
    case $stdin.gets.chomp
    when /(?:sr|survived) (.+)/i
      land = $lands.find{|land| land.name.casecmp($1) == 0 || land.code.casecmp($1) == 0}
      land.state = "done"
    when /(?:kp|skip) (.+)/i
      land = $lands.find{|land| land.name.casecmp($1) == 0 || land.code.casecmp($1) == 0}
      land.state = "skip"
    when /(?:ii|died in) (.+)/i
      land = $lands.find{|land| land.name.casecmp($1) == 0 || land.code.casecmp($1) == 0}
      land.state = "todo"
      land.deaths += 1
      restart
      p_stats
    when /ee|died en-route/i
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

open $filename, "w" do |file|
  file.puts JSON.generate $lands.map(&:save_data)
end
