
module Shuttle
  class System
    module Macros
      
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
      
      def use(adapter)
        adapter_klass  = (Shuttle::Adapters.const_get(adapter) rescue nil)
        raise "Adapter not found! (#{adapter})" unless adapter
        
        adapter_macros = (adapter_klass.const_get('Macros') rescue nil)
        extend adapter_macros if adapter_macros
        
        @adapters.push(adapter_klass)
      end
      
    end
  end
end
