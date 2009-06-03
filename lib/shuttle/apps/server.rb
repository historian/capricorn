Shuttle.runtime_gem('thor', Shuttle::THOR_VERSION)

module Shuttle
  module Apps
    
    class Server < Thor
      
      desc "start", 'start the server'
      method_options :foreground => :boolean,  :config => :optional
      def start
        Shuttle.server? true
        Shuttle::System.load!(options[:root_path])
        
        begin
          FileUtils.mkdir_p(Shuttle.system.root)
        rescue Errno::EACCES
          Shuttle.logger.out.fatal "must be executed as root"
          exit(1)
        end
        
        unless Shuttle.system.is_user('root')
          Shuttle.logger.out.fatal "must be executed as root"
          exit(1)
        end
        
        if options[:foreground]
          Shuttle::Server.start
        else
          Shuttle::Server.daemonize
        end
      end
      
      desc "stop", 'stop the server'
      method_options :token => :optional
      def stop
        Shuttle.client(options[:token]).stop_server
      end
      
      desc "restart", 'restart the server'
      method_options :token => :optional
      def restart
        Shuttle.client(options[:token]).restart_server
      end
      
      desc "reload", 'reload the server'
      method_options :token => :optional
      def reload
        Shuttle.client(options[:token]).reload_server
      end
      
      desc "update", 'update the shuttle'
      method_options :token => :optional
      def update
        Shuttle.client(options[:token]).update_server
      end
      
      desc "version", 'version of the server'
      method_options :token => :optional
      def version
        puts "Client: #{Shuttle.version}"
        puts "Server: #{Shuttle.client(options[:token]).server_version}"
      end
      
    end
    
  end
end