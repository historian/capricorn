
module Shuttle
  class System
    
    autoload :Shell,       File.dirname(__FILE__)+'/system/shell'
    autoload :Macros,      File.dirname(__FILE__)+'/system/macros'
    autoload :Options,     File.dirname(__FILE__)+'/system/options'
    autoload :Satellites,  File.dirname(__FILE__)+'/system/satellites'
    autoload :ProcessUser, File.dirname(__FILE__)+'/system/process_user'
    
    include Shuttle::System::Shell
    include Shuttle::System::Macros
    include Shuttle::System::Options
    include Shuttle::System::Satellites
    include Shuttle::System::ProcessUser
    
    # get the shared system. this will also set the shared system if you pass a new system as its argument.
    def self.shared(system=nil)
      @system = system if system
      @system
    end
    
    # load the shared system
    def self.load!(root=nil)
      @system = new(root)
    end
    
    def initialize(root=nil)
      unless root
        if get_user_name == 'root'
          root = Shuttle::DEFAULT_ROOT_SYSTEM_DIR
        else
          root = Shuttle::DEFAULT_USER_SYSTEM_DIR
        end
      end
      
      @root               = File.expand_path(root)
      @actors           = []
      @options            = {}
      @option_descriptors = []
      @satellite_options            = {}
      @satellite_option_descriptors = []
      
      system_file = self.path('system.rb')
      unless File.file? system_file
        Shuttle.log "No system file found (#{system_file})"
        exit(1)
      end
      
      use :BaseActor
      self.instance_eval File.read(system_file)
      self.resolve_options!
    end
    
    attr_accessor :current_satellite, :root
    
    def path(*args)
      File.join(@root, *args)
    end
    
    def install_satellite(domain)
      satellite = Shuttle::Satellite.new(domain)
      
      run_action_on :install_satellite, satellite
      run_action_on :link_satellite, satellite
      save_satellite! satellite
      
      satellite
    end
    
    def uninstall_satellite(satellite)
      if satellite
        run_action_on :uninstall_satellite, satellite
        
        destroy_satellite! satellite
        true
      else
        false
      end
    end
    
    def install_engine(satellite, name, options={})
      if satellite
        ensure_precense_of_gem(name, options)
        if satellite.add_engine(name, options)
          run_action_on :install_engine, satellite
          run_action_on :link_satellite, satellite
          save_satellite! satellite
          return true
        end
      end
      return false
    end
    
    def update_engine(satellite, name, options={})
      if satellite
        ensure_precense_of_gem(name, options)
        if satellite.update_engine(name, options)
          run_action_on :update_engine, satellite
          run_action_on :link_satellite, satellite
          save_satellite! satellite
          return true
        end
      end
      return false
    end
    
    def uninstall_engine(satellite, name)
      if satellite
        if satellite.remove_engine(name)
          run_action_on :uninstall_engine, satellite
          run_action_on :link_satellite, satellite
          save_satellite! satellite
          return true
        end
      end
      return false
    end
    
  private
    
    def run_action_on(action, satellite)
      resolve_options_with satellite do
        @actors.each do |actor_klass|
          actor = actor_klass.new(self, satellite)
          actor.send "run_#{action}_callbacks!"
        end
      end
    end
    
  end
end
