require 'rubygems/specification'

module Gem # :nodoc:
  class Specification
    
    def add_engine_dependency(name, options={})
      Gem.ui.alert_warning('spec.add_engine_dependency is depricated please use spec.add_rails_dependency (provided by composite).')
      add_rails_dependency(name, options={})
    end
    
    def engine_dependencies
      Gem.ui.alert_warning('spec.add_engine_dependency is depricated please use spec.add_rails_dependency (provided by composite).')
      self.rails_dependencies
    end
    
    def engine_dependencies=(value)
      Gem.ui.alert_warning('spec.add_engine_dependency is depricated please use spec.add_rails_dependency (provided by composite).')
      self.rails_dependencies = value
    end
    
  end
end