module Capr::ConfigHelpers
  
  def self.included(base)
    if Class === base
      base.send :include, InstanceMethods
    end
    base.send :extend, ClassMethods
  end
  
  module ClassMethods
  
    def config
      Capr::NODE.config
    end
    
  end
  
  module InstanceMethods
    
    def config
      Capr::NODE.config
    end
    
  end
  
end
  