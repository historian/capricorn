
module Capricorn
  class System
    include DRbUndumped
    
    autoload :Shell,       File.dirname(__FILE__)+'/system/shell'
    autoload :Config,      File.dirname(__FILE__)+'/system/config'
    autoload :Helper,      File.dirname(__FILE__)+'/system/helper'
    autoload :Options,     File.dirname(__FILE__)+'/system/options'
    autoload :Satellites,  File.dirname(__FILE__)+'/system/satellites'
    autoload :ProcessUser, File.dirname(__FILE__)+'/system/process_user'
    
    include Capricorn::System::Shell
    include Capricorn::System::Config
    include Capricorn::System::Helper
    include Capricorn::System::Options
    include Capricorn::System::Satellites
    include Capricorn::System::ProcessUser
    
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
          root = Capricorn::DEFAULT_ROOT_SYSTEM_DIR
        else
          root = Capricorn::DEFAULT_USER_SYSTEM_DIR
        end
      end
      
      @root               = File.expand_path(root)
      @actors             = []
      @options            = {}
      @option_descriptors = []
      @satellite_options            = {}
      @satellite_option_descriptors = []
      
      system_file = self.path('system.rb')
      unless File.file? system_file
        Capricorn.log "No system file found (#{system_file})"
        exit(1)
      end
      
      Capricorn::ExceptionHandler.setup(self.path('ServerNormal.log'), self.path('ServerError.log'))
      
      use :BaseActor
      self.instance_eval File.read(system_file)
      self.resolve_options!
    end
    
    attr_accessor :current_satellite, :root
    
    def queue
      @queue ||= Capricorn::JobQueue.new
    end
    
    def path(*args)
      File.join(@root, *args)
    end
    
    def install_satellite(domain, immediate)
      self.queue.enqueue("install new satellite #{domain}", :domain => domain, :immediate => immediate) do |options|
        satellite = Capricorn::Satellite.new(options[:domain])
        
        run_action_on :install_satellite, satellite
        run_action_on :link_satellite, satellite
        save_satellite! satellite
      end
    end
    
    def relink_satellite(satellite, immediate)
      self.queue.enqueue("relink #{satellite.domain}", :satellite => satellite, :immediate => immediate) do |options|
        satellite = options[:satellite]
        
        if satellite
          run_action_on :link_satellite, satellite
          save_satellite! satellite
        end
      end
    end
    
    def update_satellite(satellite, immediate)
      if satellite
        self.queue.enqueue("update #{satellite.domain}", :satellite => satellite, :immediate => immediate) do |options|
          
          satellite = options[:satellite]
          if satellite.update_all_engines
            run_action_on :update_engine, satellite
            run_action_on :link_satellite, satellite
            save_satellite! satellite
          end
          
        end
      else
        false
      end
    end
    
    def uninstall_satellite(satellite, immediate)
      if satellite
        self.queue.enqueue("uninstall #{satellite.domain}", :satellite => satellite, :immediate => immediate) do |options|
          run_action_on :uninstall_satellite, options[:satellite]
          destroy_satellite! options[:satellite]
        end
      else
        false
      end
    end
    
    def make_development_satellite(satellite, name)
      if satellite
        satellite.module_name = name.to_s
        satellite.development = true
        Capricorn.runtime_gem('rubigen', Capricorn::RUBIGEN_VERSION)
        resolve_options_with satellite do
          as_user(web_user, web_group) do
            Dir.chdir(satellite_root) do
              
              FileUtils.rm_r("doc", :verbose => true) rescue nil
              FileUtils.rm_r("README", :verbose => true) rescue nil
              FileUtils.rm_r("public/javascripts", :verbose => true) rescue nil
              FileUtils.mkdir_p("public/vendor", :verbose => true) rescue nil
              FileUtils.ln_s(
                File.join(satellite_root, "public"),
                File.join(satellite_root, "public/vendor", satellite.module_name),
                :verbose => true) rescue nil
              
              require 'rubigen/scripts/generate'
              RubiGen::Base.use_application_sources!
              RubiGen::Scripts::Generate.new.run(["-f", name], :generator => 'engine')
              
            end
          end
        end
        save_satellite! satellite
      else
        false
      end
    end
    
    def install_engine(satellite, name, options, immediate)
      if satellite
        self.queue.enqueue("install #{satellite.domain}: #{name} #{options.inspect}",
          :satellite => satellite, :name => name, :options => options, :immediate => immediate) do |options|
          
          satellite, name, options = options[:satellite], options[:name], options[:options]
          resolve_options_with(satellite) { ensure_presence_of_gem(name, options) }
          if satellite.add_engine(name, options)
            run_action_on :install_engine, satellite
            run_action_on :link_satellite, satellite
            save_satellite! satellite
          end
          
        end
      else
        false
      end
    end
    
    def update_engine(satellite, name, options, immediate)
      if satellite
        self.queue.enqueue("update #{satellite.domain}: #{name} #{options.inspect}",
          :satellite => satellite, :name => name, :options => options, :immediate => immediate) do |options|
          
          satellite, name, options = options[:satellite], options[:name], options[:options]
          resolve_options_with(satellite) { ensure_presence_of_gem(name, options) }
          if satellite.update_engine(name, options)
            run_action_on :update_engine, satellite
            run_action_on :link_satellite, satellite
            save_satellite! satellite
          end
          
        end
      else
        false
      end
    end
    
    def uninstall_engine(satellite, name, immediate)
      if satellite
        self.queue.enqueue("uninstall #{satellite.domain}: #{name}",
          :satellite => satellite, :name => name, :immediate => immediate) do |options|
          
          satellite, name = options[:satellite], options[:name]
          if satellite.remove_engine(name)
            run_action_on :uninstall_engine, satellite
            run_action_on :link_satellite, satellite
            save_satellite! satellite
          end
          
        end
      else
        false
      end
    end
    
  private
    
    def run_action_on(action, satellite)
      resolve_options_with satellite do
        actors = @actors.collect { |actor_klass| actor_klass.new(self, satellite) }
        actors.each { |actor| actor.run_callbacks_in_fase! action, :before }
        actors.each { |actor| actor.run_callbacks_in_fase! action, :on     }
        actors.each { |actor| actor.run_callbacks_in_fase! action, :after  }
      end
    end
    
  end
end
