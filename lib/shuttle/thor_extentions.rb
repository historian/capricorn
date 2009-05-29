
module Thor::Util
  class << self
    alias_method :old_constant_to_thor_path, :constant_to_thor_path
    alias_method :old_constant_from_thor_path, :constant_from_thor_path
  end
  
  def self.constant_to_thor_path(*args)
    path = old_constant_to_thor_path(*args)
    path.sub! /^#{Thor.namespace}:/, '' if Thor.namespace
    path
  end
  
  def self.constant_from_thor_path(path)
    path = "#{Thor.namespace}:"+path if Thor.namespace
    old_constant_from_thor_path(path)
  end
end

class Thor
  class << self
    attr_accessor :real_namespace
    def namespace=(value)
      Thor.real_namespace = value
    end
    def namespace
      Thor.real_namespace
    end
  end
  self.namespace = nil
end
