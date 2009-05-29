
module Shuttle
  class System
    
    autoload :Shell,       File.dirname(__FILE__)+'/system/shell'
    autoload :Macros,      File.dirname(__FILE__)+'/system/macros'
    autoload :Options,     File.dirname(__FILE__)+'/system/options'
    autoload :ProcessUser, File.dirname(__FILE__)+'/system/process_user'
    
    include Shuttle::System::Shell
    include Shuttle::System::Macros
    include Shuttle::System::Options
    include Shuttle::System::ProcessUser
    
    def self.shared
      self.load! unless @system
      @system
    end
    
    def self.load!
      system_file = Shuttle.config.path('system.rb')
      unless File.file? system_file
        Shuttle.log "No system file found (#{system_file})"
        exit(1)
      end
      @system = new
      @system.instance_eval File.read(system_file)
      @system.resolve_options!
    end
    
    def initialize
      @adapters           = []
      @options            = {}
      @option_descriptors = []
      use :BaseAdapter
    end
    
    def install(satellite)
      resolve_options! satellite
      @adapters.each do |adapter_klass|
        adapter = adapter_klass.new(satellite)
        adapter.run_install_callbacks!
      end
    end
    
    def link(satellite)
      resolve_options! satellite
      @adapters.each do |adapter_klass|
        adapter = adapter_klass.new(satellite)
        adapter.run_link_callbacks!
      end
    end
    
    def uninstall(satellite)
      resolve_options! satellite
      @adapters.each do |adapter_klass|
        adapter = adapter_klass.new(satellite)
        adapter.run_uninstall_callbacks!
      end
    end
    
    def find_path(*paths)
      options = paths.pop if Hash === paths.last
      options ||= {}
      search_paths = options.delete(:search) || '/'
      search_paths = [search_paths].flatten.compact.uniq
      
      paths = paths.flatten.compact.uniq
      
      search_paths.each do |search_path|
        paths.each do |path|
          results = Dir.glob(File.join(search_path, path))
          return results.first if results.first
        end
      end
      
      return nil
    end
    
  end
end
