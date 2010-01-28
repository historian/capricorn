require File.dirname(File.expand_path(__FILE__))+'/../../minigems'
require 'rush'
require 'mustache'

class SystemContext
  
  def self.run(script, attributes={})
    new(attributes).load(script)
  end
  
  def initialize(attributes={})
    @binding = binding
    @attributes = {}
    
    attributes[:box] ||= Rush::Box.new('localhost')
    attributes.each do |name, value|
      set(name, value)
    end
  end
  
  def run(script, attributes={})
    self.class.new(script, @attributes.merge(attributes))
  end
  
  def load(script)
    case script
    when IO then eval(script.read, @binding)
    when String then
      if File.file?(script)
        eval(File.read(script), @binding, script, 0)
      else
        eval(script, @binding)
      end
    end
    self
  end
  
  def set(attribute, value)
    unless self.respond_to?(attribute)
      mc = (class << self ; self ; end)
      mc.send(:define_method, attribute.to_sym) { @attributes[attribute.to_sym] }
    end
    @attributes[attribute.to_sym] = value
  end
  
  def render(tpl, context={})
    Mustache.render(tpl, @attributes.merge(context))
  end
  
end