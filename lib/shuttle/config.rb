
module Shuttle
  class Config
    
    attr_accessor :root
    
    def initialize(root)
      @root = File.expand_path(root)
      @config = {}
      unless File.directory? @root
        FileUtils.mkdir_p(@root)
      end
      if File.file? self.path('config.yml')
        @config = YAML.load_file(self.path('config.yml'))
      end
    end
    
    def path(*args)
      File.join(@root, *args)
    end
    
    def save!
      File.open(self.path('config.yml'), 'w+') do |f|
        f.write YAML.dump(@config)
      end
    end
    
    def [](key)
      @config[key]
    end
    
    def []=(key, value)
      @config[key] = value
    end
    
    def each(&block)
      @config.each(&block)
    end
    
  end
end