
module Shuttle
  module Adapters
    class MysqlAdapter < Shuttle::Adapter
      
      on_install   :create_database
      on_uninstall :drop_database
      
      def create_database
        
      end
      
      def drop_database
        
      end
      
    end
  end
end
