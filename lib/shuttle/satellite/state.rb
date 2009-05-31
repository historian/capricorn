
module Shuttle
  class Satellite
    module State
      
      def needs_link?
        @needs_link || false
      end
      
      def needs_link!(value=true)
        @needs_link = value
      end
      
      def needs_install?
        @needs_install || false
      end
      
      def needs_install!(value=true)
        needs_link!(true) if value
        @needs_install = value
      end
      
      def needs_uninstall?
        @needs_uninstall || false
      end
      
      def needs_uninstall!(value=true)
        @needs_uninstall = value
      end
      
    end
  end
end