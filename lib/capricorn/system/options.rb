
module Capricorn
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
            @options[name.to_sym] = handler.call(nil)
          end
          @options[name.to_sym]
        end
      end
      
      def satellite_option(name, proc, &handler)
        if proc
          @satellite_option_descriptors.push({
            :name => name.to_sym,
            :proc => proc,
            :handler => handler
          })
        else
          if handler && !@satellite_options.key?(name.to_sym)
            @satellite_options[name.to_sym] = handler.call(@current_satellite, nil)
          end
          @satellite_options[name.to_sym]
        end
      end
      
      def set_satellite_option(name, value)
        @satellite_options[name.to_sym] = value
      end
      
      def resolve_options_with(satellite)
        @current_satellite = satellite
        resolve_options!
        
        result = yield
        
        @current_satellite = nil
        resolve_options!
        
        result
      end
      
      def resolve_options!
        resolve_options_for_system!
        resolve_options_for_satellite!
      end
      
      def resolve_options_for_system!
        @options = {}
        @option_descriptors.each do |option|
          value = option[:proc].call           if option[:proc]
          value = option[:handler].call(value) if option[:handler]
          @options[option[:name]] = value
        end
      end
      
      def resolve_options_for_satellite!
        @satellite_options = {}
        if @current_satellite
          @satellite_option_descriptors.each do |option|
            value = option[:proc].call(@current_satellite)           if option[:proc]
            value = option[:handler].call(@current_satellite, value) if option[:handler]
            @satellite_options[option[:name]] = value
          end
        end
      end
      
    end
  end
end
