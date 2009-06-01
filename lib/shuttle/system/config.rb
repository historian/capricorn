
module Shuttle
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
      
    end
  end
end
