require 'thor'
require 'simple-daemon'

module Shuttle
  module App # :nodoc:
    
    class Server < Thor
      
      desc "start", 'start the server'
      method_options :foreground => :boolean,  :config => :optional
      def start
        Shuttle.server? true
        Shuttle::System.load!(options[:root_path])
        
        begin
          FileUtils.mkdir_p(Shuttle.system.root)
        rescue Errno::EACCES
          puts "must be executed as root"
          exit(1)
        end
        
        unless Shuttle.system.is_user('root')
          puts "must be executed as root"
          exit(1)
        end
        
        SimpleDaemon.const_set 'WORKING_DIRECTORY', Shuttle.system.root
        if options[:foreground]
          Shuttle::Server.start
        else
          ARGV.clear and ARGV.concat(%w( start ))
          Shuttle::Server.daemonize
        end
      end
      
      desc "stop", 'stop the server'
      def stop
        Shuttle.client.stop_server
      end
      
      desc "restart", 'restart the server'
      def restart
        Shuttle.client.restart_server
      end
      
      desc "reload", 'reload the server'
      def reload
        Shuttle.client.reload_server
      end
      
      desc "update", 'update the shuttle'
      def update
        Shuttle.client.update_server
      end
      
      desc "version", 'version of the server'
      def version
        puts "Client: #{Shuttle.version}"
        puts "Server: #{Shuttle.client.server_version}"
      end
      
    end
    
    class Satellite < Thor
      desc 'list', 'show all managed satellites'
      def list
        Shuttle.client.satellites.each do |sat|
          puts sat.domain
          sat.engines.each do |name, options|
            puts "- #{name} #{options.inspect}"
          end
        end
      end
      
      desc 'install DOMAIN', 'install a satellite'
      def install(domain)
        Shuttle.client.install_satellite(domain)
      end
      
      desc 'uninstall DOMAIN', 'uninstall a satellite'
      def uninstall(domain)
        Shuttle.client.uninstall_satellite(domain)
      end
    end
    
    class Engines < Thor
      desc 'install DOMAIN NAME', 'install an engine'
      method_options :version => :required, :lib => :optional, :source => :optional
      def install(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Shuttle.client.install_engine(domain, name, desc)
      end
      
      desc 'update DOMAIN NAME', 'update an engine'
      method_options :version => :required, :lib => :optional, :source => :optional
      def update(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Shuttle.client.update_engine(domain, name, desc)
      end
      
      desc 'uninstall DOMAIN NAME', 'uninstall an engine'
      def uninstall(domain, name)
        Shuttle.client.uninstall_engine(domain, name)
      end
    end
    
    class Dev < Thor
      desc "create NAME", "create a new engine"
      def create(name)
        require 'rubigen'
        system("rails #{name}")
        
        FileUtils.rm_r("#{name}/doc", :verbose => true)
        FileUtils.rm_r("#{name}/README", :verbose => true)
        FileUtils.rm_r("#{name}/public/javascripts", :verbose => true)
        
        require 'rubigen/scripts/generate'
        RubiGen::Base.use_application_sources!
        RubiGen::Scripts::Generate.new.run(["-f", name], :generator => 'engine')
      end
      
      desc "link", "link the current development app"
      def link
      end
    end
    
  end
end