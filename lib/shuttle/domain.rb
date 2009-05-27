
module Shuttle
  class Domain
    
    def self.all
      self.load! unless @domains
      @domains.values
    end
    
    def self.find(domain)
      self.load! unless @domains
      @domains[domain]
    end
    
    def self.save!
      File.open(Shuttle.config.data_path('domains.yml'), 'w+') do |f|
        f.write YAML.dump(@domains)
      end
    end
    
    def self.load!
      @domains = {}
      if File.exist? Shuttle.config.data_path('domains.yml')
        @domains = YAML.load_file(Shuttle.config.data_path('domains.yml'))
      end
    end
    
    def self.add_domain(domain)
      @domains[domain.domain] = domain
    end
    
    attr_reader :domain, :engines
    
    def initialize(domain)
      @domain = domain
      @engines = {}
      @needs_link = false
      @needs_setup = true
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
    
    def needs_setup?
      @needs_setup || false
    end
    
    def needs_setup!(value=true)
      needs_link!(true) if value
      @needs_setup = value
    end
    
    def to_yaml(*args)
      private_vars = %w( needs_link needs_setup ).inject({}) do |m, name|
        m[name] = instance_variable_get("@#{name}".to_sym)
        instance_variable_remove("@#{name}".to_sym)
      end
      
      yaml = super(*args)
      
      private_vars.each do |name, value|
        instance_variable_set("@#{name}".to_sym, value)
      end
      
      yaml
    end
    
  end
end