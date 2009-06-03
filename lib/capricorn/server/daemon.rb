
module Capricorn
  class Server
    
    module Daemon # :nodoc:
      
      def self.included(base)
        base.extend Capricorn::Server::Daemon::ClassMethods
      end
      
      # all a daemon needs to run
      module ClassMethods
        
        # construct a Capricorn uri from a DRb uri
        def construct_uri(uri)
          uri        = URI.parse(uri)
          uri.scheme = ( Capricorn.system.use_ssl? ? 'ssl+capricorn' : 'capricorn')
          uri.to_s
        end
        
        # stop the server
        def stop
          Capricorn.client.stop_server
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
            when Capricorn::STOP_STATUS
              stop_server = true
              retries     = 0
            when Capricorn::RESTART_STATUS
              wait_before_start = 2
              retries     = 0
            when Capricorn::RELOAD_STATUS
              stop_server = true
              retries     = 0
              Capricorn::Daemon::PidFile.destroy
              Capricorn.system.run %{#{Capricorn::BIN_PATH} #{ORIGINAL_ARGV.join(' ')}}
            else
              retries += 1
              stop_server = true if retries >= 3
            end
          end
        end
        
        # start the actual DRb server
        def run_server
          $master = false
          Dir.chdir(Capricorn.system.root)
          
          Capricorn.log "Server started"
          uri = "druby://#{Capricorn.system.server_hostname}:#{Capricorn.system.server_port}"
          DRb.start_service uri, self.proxy, self.options_for_server
          Capricorn.log "listening at #{self.construct_uri(uri)}"
          make_client_cert_public!
          
          at_exit do
            Capricorn.system.queue.stop!
            Capricorn.log "Server stopped"
          end
          
          DRb.thread.join
          exit($exitstatus || Capricorn::STOP_STATUS)
        end
        
      end
    end
  end
end
