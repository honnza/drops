require 'nokogiri'
NX = Nokogiri::XML

# Class definitions ############################################################

class Tag
  attr_accessor :name, :tag_rules, :attr_rules, :cdata_always, :cdata_sometimes
  def initialize name
    @name = name
    @tag_rules = {}
    @attr_rules = {}
    @cdata_always = true
    @cdata_sometimes = false
  end
end

class TagRule
  attr_accessor :parent, :child, :min_arity, :max_arity
  def initialize parent, child
    @parent = parent
    @child = child
    @min_arity = Float::INFINITY
    @max_arity = 0
  end
end

class AttrRule
  attr_accessor :tag, :attr, :optional
  def initialize tag, attr
    @tag = tag
    @attr = attr
    @optional = false
  end
end

# Core #########################################################################

class DTDBuilder
  attr_accessor :tags, :root_tag
  def initialize
    @tags = {}
    @root_tag = Tag.new nil
  end

  def add_documents *documents
    documents = documents[0] if Array === documents[0]
    documents.each{|d|add_document d}
    self
  end

  def add_document document
    document = NX.parse document if String === document
#   document = document.root if NX::Document === document
    [document, *document.css(?*)].each do |parent|
      parent_tag = if   NX::Document === parent
                   then @root_tag
                   else (@tags[parent.name] ||= Tag.new parent.name)
                   end
      cdata = parent.children.any? &:text?
      parent_tag.cdata_sometimes ||= cdata
      parent_tag.cdata_always &&= cdata
      children_by_name = parent.element_children.group_by(&:name)
      parent_tag.tag_rules.each_value do |rule|
        rule.min_arity = 0 unless children_by_name.has_key? rule.child
      end
      children_by_name.each do |name, children|
        rule = (parent_tag.tag_rules[name] ||= TagRule.new parent.name, name)
        rule.max_arity = [rule.max_arity, children.count].max
        rule.min_arity = [rule.min_arity, children.count].min
      end
      parent_tag.attr_rules.each_value do |rule|
        rule.optional = true unless parent.has_attribute? rule.attr
      end
      parent.attribute_nodes.each do |attr|
        parent_tag.attr_rules[attr.name] ||= AttrRule.new parent.name, attr.name
      end
    end
  end
end

# Front-end ####################################################################

def list_results result_set
  [result_set.root_tag, *result_set.tags.values].each do |tag|
    puts
    puts "#{tag.name || "<root>"}"
    puts "  attributes:" unless tag.attr_rules.empty?
    tag.attr_rules.each_value do |rule|
      puts "    #{rule.attr}#{"  optional" if rule.optional}"
    end
    puts "  children:" unless tag.tag_rules.empty?
    tag.tag_rules.each_value do |rule|
      puts "    #{rule.child} (#{rule.min_arity}:#{rule.max_arity})"
    end
    puts "  #{if  tag.cdata_always    then "cdata required"
           elsif !tag.cdata_sometimes then "cdata not present"
           else                            "cdata allowed"
           end}"
  end
end

results = DTDBuilder.new
loop do
  puts "what document to parse? Empty line to stop:"
  filemask = gets.chomp
  break if filemask.empty?
  filenames = Dir[filemask]
  puts "No such file was found" if filenames.empty?
  filenames.each do |filename|
    begin
      puts "parsing #{filename}"
      results.add_document IO.read filename
    rescue =>e
      puts e.inspect
      puts e.backtrace
    end
  end
  puts "partial results:"
  list_results results
end