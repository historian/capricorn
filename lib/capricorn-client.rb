module Capricorn
  require 'thor'
  require 'yaml'
  require 'bertrpc'
  require 'highline'
  require 'fileutils'
  
  require 'capricorn-client/helpers'
  
  module CLI
    require 'capricorn-client/cli/applications'
    require 'capricorn-client/cli/gems'
    require 'capricorn-client/cli/machines'
  end
  
  module Runner
    
    HELP = <<-EOH
Topics:
  CLUSTER applications
  # manage applications
   
  CLUSTER gems
  # manage gems
   
  CLUSTER machines
  # manage machines
EOH
    
    DEFAULT_CONFIG = <<-EOH
cluster_name:
  host: localhost
  port: 3457
  username: dummy
  password: dummy
EOH
    
    def start
      cluster_name = ARGV.shift
      unless cluster_name
        help
        exit 1
      end
      
      @cluster = config[cluster_name]
      unless @cluster
        puts "I don't know this cluster"
        exit 1
      end
      
      case ARGV.shift
      when 'applications'
        Capricorn::CLI::Applications.start
      when 'gems'
        Capricorn::CLI::Gems.start
      when 'machines'
        Capricorn::CLI::Machines.start
      else
        help
      end
    end
    
    def help
      puts HELP
    end
    
    def cluster
      @cluster
    end
    
    def config
      @config ||= begin
        config_path = File.expand_path('~/.capricorn/config.yml')
        unless File.file?(config_path)
          FileUtils.mkdir_p(File.dirname(config_path))
          File.open(config_path, 'w+', 0600) { |file| file.write DEFAULT_CONFIG }
          puts "Please edit your config file in ~/.capricorn/config.yml"
          exit 2
        end
        YAML.load_file(config_path)
      end
    end
    
    extend self
  end
end