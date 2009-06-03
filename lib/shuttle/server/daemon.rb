
module Shuttle
  class Server
    
    module Daemon # :nodoc:
      
      def self.included(base)
        base.extend Shuttle::Server::Daemon::ClassMethods
      end
      
      # all a daemon needs to run
      module ClassMethods
        
        # construct a Shuttle uri from a DRb uri
        def construct_uri(uri)
          uri        = URI.parse(uri)
          uri.scheme = ( Shuttle.system.use_ssl? ? 'ssl+shuttle' : 'shuttle')
          uri.to_s
        end
        
        # stop the server
        def stop
          Shuttle.client.stop_server
        end
        
        # start the server
        def start
          start_with_failsafe
        end
        
        # start the failsafe runner.
        def start_with_failsafe
          $master = true
          stop_server = false
          wait_before_start = 0
          retries = 0
          until stop_server
            if wait_before_start > 0
              sleep(wait_before_start)
              wait_before_start = 0
            end
            
            pid = Process.fork { self.run_server }
            return unless pid
            Process.waitpid(pid, 0)
            case $?.exitstatus
            when Shuttle::STOP_STATUS
              stop_server = true
              retries     = 0
            when Shuttle::RESTART_STATUS
              wait_before_start = 2
              retries     = 0
            when Shuttle::RELOAD_STATUS
              stop_server = true
              retries     = 0
              Shuttle::Daemon::PidFile.destroy
              Shuttle.system.run %{#{Shuttle::BIN_PATH} #{ORIGINAL_ARGV.join(' ')}}
            else
              retries += 1
              stop_server = true if retries >= 3
            end
          end
        end
        
        # start the actual DRb server
        def run_server
          $master = false
          Dir.chdir(Shuttle.system.root)
          
          Shuttle.log "Server started"
          uri = "druby://#{Shuttle.system.server_hostname}:#{Shuttle.system.server_port}"
          DRb.start_service uri, self.proxy, self.options_for_server
          Shuttle.log "listening at #{self.construct_uri(uri)}"
          make_client_cert_public!
          
          at_exit do
            Shuttle.system.queue.stop!
            Shuttle.log "Server stopped"
          end
          
          DRb.thread.join
          exit($exitstatus || Shuttle::STOP_STATUS)
        end
        
      end
    end
  end
end
