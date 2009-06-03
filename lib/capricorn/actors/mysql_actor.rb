
module Capricorn
  module Actors # :nodoc:
    class MysqlActor < Capricorn::Actor
      
      on_install_satellite       :create_database
      before_uninstall_satellite :backup_database
      on_uninstall_satellite     :drop_database
      
      def create_database
        
      end
      
      def drop_database
        
      end
      
    end
  end
end
