
module Capricorn
  class Server
    # the proxy object hides all the server internals from the clients.
    class Proxy
      
      def initialize(server)
        @server = server
      end
      
      def self.allow(*methods)
        methods.each do |method|
          module_eval %{ def #{method}(*args,&block) ; @server.#{method}(*args,&block) ; end }
        end
      end
      
      allow :stop_server, :restart_server, :reload_server, :server_version, :update_server, :install_satellite, :uninstall_satellite, :install_engine, :update_engine, :uninstall_engine, :satellites, :queued_jobs, :cancel_job, :immediate_job, :make_development_satellite, :update_gems, :relink_satellite, :update_satellite
      
      class << self
        undef_method :allow
      end
      
    end
  end
end