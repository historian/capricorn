
module Shuttle
  class System
    
    def self.shared
      self.load! unless @system
      @system
    end
    
    def self.load!
      system_file = Shuttle.config.path('system.rb')
      unless File.file? system_file
        puts "No system file found (#{system_file})"
        exit(1)
      end
      @system = new
      @system.instance_eval File.read(system_file)
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
    
  private
    
    def use(adapter)
      adapter_klass  = (Shuttle::Adapters.const_get(adapter) rescue nil)
      raise "Adapter not found! (#{adapter})" unless adapter
      
      adapter_macros = (adapter_klass.const_get('Macros') rescue nil)
      extend adapter_macros if adapter_macros
      
      @adapters.push(adapter_klass)
    end
    
    def option(name, proc, &handler)
      if proc
        @option_descriptors.push({
          :name => name.to_sym,
          :proc => proc,
          :handler => handler
        })
      else
        @options[name.to_sym]
      end
    end
    
    def resolve_options!(satellite)
      @options = {}
      @option_descriptors.each do |option|
        value = option[:proc].call(satellite)
        value = option[:handler].call(satellite, value) if option[:handler]
        @options[option[:name]] = value
      end
    end
    
  end
end
