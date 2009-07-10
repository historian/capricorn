Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps
    
    class Server < Thor
      
      desc "start", 'start the server'
      method_options :foreground => :boolean,  :config => :optional
      def start
        Capricorn.server? true
        Capricorn::System.load!(options[:root_path])
        
        begin
          FileUtils.mkdir_p(Capricorn.system.root)
        rescue Errno::EACCES
          Capricorn.logger.out.fatal "must be executed as root"
          exit(1)
        end
        
        unless Capricorn.system.is_user('root')
          Capricorn.logger.out.fatal "must be executed as root"
          exit(1)
        end
        
        if options[:foreground]
          Capricorn::Server.start
        else
          Capricorn::Server.daemonize
        end
      end
      
      desc "stop", 'stop the server'
      method_options :token => :optional
      def stop
        Capricorn.client(options[:token]).stop_server
      end
      
      desc "restart", 'restart the server'
      method_options :token => :optional
      def restart
        Capricorn.client(options[:token]).restart_server
      end
      
      desc "reload", 'reload the server'
      method_options :token => :optional
      def reload
        Capricorn.client(options[:token]).reload_server
      end
      
      desc "update", 'update the capricorn'
      method_options :token => :optional
      def update
        Capricorn.client(options[:token]).update_server
      end
      
      desc "gupdate", 'update the gems'
      method_options :token => :optional
      def gupdate
        Capricorn.client(options[:token]).update_gems
      end
      
      desc "version", 'version of the server'
      method_options :token => :optional
      def version
        puts "Client: #{Capricorn.version}"
        puts "Server: #{Capricorn.client(options[:token]).server_version}"
      end
      
    end
    
  end
end