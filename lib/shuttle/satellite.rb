
module Shuttle
  class Satellite
    
    def self.all
      self.load! unless @satellites
      @satellites.values
    end
    
    def self.find(satellite)
      self.load! unless @satellites
      @satellites[satellite]
    end
    
    def self.save!
      File.open(Shuttle.config.path('satellites.yml'), 'w+') do |f|
        satellites = @satellites.values.collect do |satellite|
          unless satellite.needs_uninstall?
            satellite.for_yaml
          end
        end
        f.write YAML.dump(satellites.compact)
      end
    end
    
    def self.load!
      @satellites = {}
      if File.exist? Shuttle.config.path('satellites.yml')
        satellites = YAML.load_file(Shuttle.config.path('satellites.yml'))
        satellites.each do |data|
          satellite = Shuttle::Satellite.new(data)
          @satellites[satellite.domain] = satellite
        end
      end
    end
    
    def self.add_satellite(satellite)
      self.load! unless @satellites
      satellite.needs_install!
      @satellites[satellite.domain] = satellite
    end
    
    def self.remove_satellite(satellite)
      self.load! unless @satellites
      @satellites.delete(satellite.domain)
    end
    
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
    
    def add_engine(name, options={})
      unless @engines.key? name
        @engines[name] = options
        @needs_link = true
      end
    end
    
    def update_engine(name, options={})
      if @engines.key? name
        @engines[name] = options
        @needs_link = true
      end
    end
    
    def remove_engine(name)
      if @engines.key? name
        @engines.delete(name)
        @needs_link = true
      end
    end
    
    def needs_link?
      @needs_link || false
    end
    
    def needs_link!(value=true)
      @needs_link = value
    end
    
    def needs_install?
      @needs_install || false
    end
    
    def needs_install!(value=true)
      needs_link!(true) if value
      @needs_install = value
    end
    
    def needs_uninstall?
      @needs_uninstall || false
    end
    
    def needs_uninstall!(value=true)
      @needs_uninstall = value
    end
    
    def for_yaml
      attributes_for_yaml = {}
      
      private_vars = %w( needs_link needs_install needs_uninstall )
      instance_variables.each do |ivar_name|
        ivar_name = ivar_name.to_s
        ivar_name =~ /^@(.+)$/
        name = $1
        unless private_vars.include? name
          attributes_for_yaml[name] = instance_variable_get(ivar_name.to_sym)
        end
      end
      
      attributes_for_yaml
    end
    
  end
end