
module Capricorn
  
  # The actor class provides the mechanisme for scheduling the execution of seperate tasks.
  class Actor
    
    autoload :Actions, File.dirname(__FILE__)+'/actor/actions'
    
    include Capricorn::Actor::Actions
    
    attr_reader :system, :satellite
    
    action :install_satellite, :uninstall_satellite, :link_satellite
    action :install_engine,    :uninstall_engine,    :update_engine
    
    # create a new actor for the provided system and satellite
    def initialize(system, satellite)
      @system    = system
      @satellite = satellite
    end
    
  end
end