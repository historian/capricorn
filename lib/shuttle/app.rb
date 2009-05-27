
module Shuttle
  class App < Thor
    
    method_options :config => :optional
    def initialize(opts = {}, *args)
      super
      if opts[:config]
        @root_path = opts[:config]
      else
        args.each_with_index do |arg, idx|
          if arg =~ /^(-c|--config)$/
            @root_path = args[idx+1]
          elsif arg =~ /^(-c|--config)=(.+)$/
            @root_path = $2
          end
        end
      end
      
      Shuttle.app(self)
      Shuttle.config(@root_path || Shuttle::DEFAULT_CONFIG_DIR)
    end
    
    desc "start", 'start the server'
    method_options :foreground => :boolean
    def start
      SimpleDaemon.const_set 'WORKING_DIRECTORY', Shuttle.config.root
      if options[:foreground]
        Shuttle::Server.start
      else
        ARGV.clear and ARGV.concat(%w( start ))
        Shuttle::Server.daemonize
      end
    end
    
    desc "stop", 'stop the server'
    def stop
      SimpleDaemon.const_set 'WORKING_DIRECTORY', Shuttle.config.root
      ARGV.clear and ARGV.concat(%w( stop ))
      Shuttle::Server.daemonize
    end
    
    desc "restart", 'restart the server'
    def restart
      SimpleDaemon.const_set 'WORKING_DIRECTORY', Shuttle.config.root
      ARGV.clear and ARGV.concat(%w( restart ))
      Shuttle::Server.daemonize
    end
    
    desc 'env', 'show the environment'
    def env
      Shuttle.config.each do |key, value|
        puts "#{key}: #{value.inspect}"
      end
    end
    
    desc 'satellites', 'show all managed satellites'
    def satellites
      client.satellites.each do |sat|
        puts sat.domain
        sat.engines.each do |name, options|
          puts "- #{name} #{options.inspect}"
        end
      end
    end
    
    desc 'install DOMAIN', 'install a satellite'
    def install(domain)
      client.install_satellite(domain)
    end
    
    desc 'uninstall DOMAIN', 'uninstall a satellite'
    def uninstall(domain)
      client.uninstall_satellite(domain)
    end
    
    desc 'einstall DOMAIN NAME', 'install an engine'
    method_options :version => :required, :lib => :optional, :source => :optional
    def einstall(domain, name)
      desc = { :version => options[:version] }
      desc[:lib]    = options[:lib]    if options[:lib]
      desc[:source] = options[:source] if options[:source]
      client.install_engine(domain, name, desc)
    end
    
  private
    
    def client
      unless @client
        @client = Shuttle::Client.shared
        unless @client
          puts "Failed to connect to the shuttle!"
          exit(1)
        end
      end
      @client
    end
    
  end
end