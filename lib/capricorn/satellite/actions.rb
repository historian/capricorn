
module Capricorn
  class Satellite
    module Actions
      
      def add_engine(name, options={})
        unless @engines.key? name
          @engines[name] = options
          true
        else
          false
        end
      end
      
      def update_engine(name, options={})
        if @engines.key? name
          @engines[name] = options
          true
        else
          false
        end
      end
      
      def remove_engine(name)
        if @engines.key? name
          @engines.delete(name)
          true
        else
          false
        end
      end
      
    end
  end
end