
module Shuttle
  class Server
    class Proxy
      
      def initialize(server)
        @server = server
      end
      
      def self.allow(*methods)
        methods.each do |method|
          module_eval %{ def #{method}(*args,&block) ; @server.#{method}(*args,&block) ; end }
        end
      end
      
      allow :stop_server, :restart_server, :reload_server, :server_version, :update_server, :install_satellite, :uninstall_satellite, :install_engine, :update_engine, :uninstall_engine, :satellites
      
      class << self
        undef_method :allow
      end
      
    end
  end
end