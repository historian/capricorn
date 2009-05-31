
module Shuttle
  class Satellite
    
    autoload :State,            File.dirname(__FILE__)+'/satellite/state'
    autoload :Actions,          File.dirname(__FILE__)+'/satellite/actions'
    autoload :Persistence,      File.dirname(__FILE__)+'/satellite/persistence'
    autoload :DependencyLoader, File.dirname(__FILE__)+'/satellite/dependency_loader'
    
    include Shuttle::Satellite::State
    include Shuttle::Satellite::Actions
    include Shuttle::Satellite::Persistence
    
    attr_reader :domain, :engines
    
    def initialize(domain)
      if Hash === domain
        domain.each do |name, value|
          instance_variable_set("@#{name}".to_sym, value)
        end
      else
        @domain = domain
        @engines = {}
      end
      @needs_install = false
      @needs_uninstall = false
      @needs_link = false
    end
    
  end
end