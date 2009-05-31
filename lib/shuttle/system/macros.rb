
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
      
      def use(actor)
        actor_klass  = (Shuttle::Actors.const_get(actor) rescue nil)
        raise "Actor not found! (#{actor})" unless actor_klass
        
        actor_macros = (actor_klass.const_get('Macros') rescue nil)
        extend actor_macros if actor_macros
        
        @actors.push(actor_klass)
      end
      
    end
  end
end
