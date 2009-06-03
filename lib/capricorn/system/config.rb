
module Capricorn
  class System
    module Config
      
      def use_development!
        environment { 'development' }
      end
      
      def development?
        environment == 'development'
      end
      
      def use_production!
        environment { 'production' }
      end
      
      def production?
        environment == 'production'
      end
      
      def environment(&block)
        option(:environment, block) { |s,v| v or 'production' }
      end
      
      def use_ssl!
        option(:use_ssl, lambda { true })
      end
      
      def use_ssl?
        option(:use_ssl, nil)
      end
      
      def bind(hostname=nil, port=nil)
        server_hostname { hostname }
        server_port     { port }
      end
      
      def server_hostname(&block)
        option(:server_hostname, block) { |v| v or 'localhost' }
      end
      
      def server_port(&block)
        option(:server_port, block) { |v| v or 5000 }
      end
      
    end
  end
end
