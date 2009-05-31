
module Shuttle
  class Actor
    
    autoload :Actions, File.dirname(__FILE__)+'/actor/actions'
    
    include Shuttle::Actor::Actions
    
    attr_reader :system, :satellite
    
    action :install_satellite, :uninstall_satellite, :link_satellite
    action :install_engine,    :uninstall_engine,    :update_engine
    
    def initialize(system, satellite)
      @system    = system
      @satellite = satellite
    end
    
  end
end