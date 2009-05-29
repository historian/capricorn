
module Shuttle
  class System
    module Options
      
      def option(name, proc, &handler)
        if proc
          @option_descriptors.push({
            :name => name.to_sym,
            :proc => proc,
            :handler => handler
          })
        else
          if handler && !@options.key?(name.to_sym)
            @options[name.to_sym] = handler.call(@current_satellite, nil)
          end
          @options[name.to_sym]
        end
      end
      
      def resolve_options!(satellite=nil)
        @current_satellite = satellite if satellite
        
        @options = {}
        @option_descriptors.each do |option|
          value = nil
          begin
            value = option[:proc].call(@current_satellite) if option[:proc]
          rescue ; end
          begin
            value = option[:handler].call(@current_satellite, value) if option[:handler]
          rescue ; end
          @options[option[:name]] = value
        end
      end
      
    end
  end
end
