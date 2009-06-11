Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

class Thor
  class << self
    attr_accessor :real_namespace # :nodoc:
  end
  def self.namespace=(value)
    Thor.real_namespace = value
  end
  def self.namespace
    Thor.real_namespace
  end
  self.namespace = nil
end

module Thor::Util # :nodoc:
  class << self
    alias_method :old_constant_to_thor_path, :constant_to_thor_path
    alias_method :old_constant_from_thor_path, :constant_from_thor_path
  end
  
  def self.constant_to_thor_path(*args) # :nodoc:
    path = old_constant_to_thor_path(*args)
    path.sub!(/^#{Thor.namespace}:/, '') if Thor.namespace
    path
  end
  
  def self.constant_from_thor_path(path) # :nodoc:
    path = "#{Thor.namespace}:"+path if Thor.namespace
    old_constant_from_thor_path(path)
  end
end
