
module Capricorn
  class System
    module Helper
      
      def use(actor)
        actor_klass  = (Capricorn::Actors.const_get(actor) rescue nil)
        raise "Actor not found! (#{actor})" unless actor_klass
        
        actor_helper = (actor_klass.const_get('Helper') rescue nil)
        extend actor_helper if actor_helper
        
        actor_config = (actor_klass.const_get('Config') rescue nil)
        extend actor_config if actor_config
        
        @actors.push(actor_klass)
      end
      
    end
  end
end
