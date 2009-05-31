require 'uri'

module Shuttle
  class Server
    module Daemon
      
      def self.included(base)
        base.extend Shuttle::Server::Daemon::ClassMethods
      end
      
      module ClassMethods
        
        def construct_uri(uri)
          uri        = URI.parse(uri)
          uri.scheme = ( Shuttle.system.use_ssl? ? 'ssl+shuttle' : 'shuttle')
          uri.to_s
        end
        
        def stop
          Shuttle.client.stop_server
        end
        
        def start
          start_with_failsafe
        end
        
        def start_with_failsafe
          stop_server = false
          wait_before_start = 0
          retries = 0
          until stop_server
            if wait_before_start > 0
              sleep(wait_before_start)
              wait_before_start = 0
            end
            
            pid = Process.fork { self.run_server }
            Process.waitpid(pid, 0)
            case $?.exitstatus
            when Shuttle::STOP_STATUS
              stop_server = true
            when Shuttle::RESTART_STATUS
              wait_before_start = 2
            when Shuttle::RELOAD_STATUS
              stop_server = true
              Shuttle.system.run(%{sleep 2 ; #{Shuttle::BIN_PATH} #{ORIGINAL_ARGV.join(' ')}})
            else
              retries += 1
              stop_server = true if retries >= 3
            end
          end
          
          path = Shuttle.system.path('Server.pid')
          File.unlink(path) if File.file?(path)
        end
        
        def run_server
          Dir.chdir(Shuttle.system.root)
          
          Shuttle.log "Server started"
          DRb.start_service 'druby://localhost:5000', self.proxy, self.options_for_server
          Shuttle.log "listening at #{self.construct_uri(DRb.uri)}"
          make_client_cert_public!
          
          at_exit do
            unless $task_child
              Shuttle.log "Server stopped"
            end
          end
          
          DRb.thread.join
        end
        
      end
    end
  end
end
