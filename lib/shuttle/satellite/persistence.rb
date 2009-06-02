require 'yaml'

module Shuttle
  class Satellite
    module Persistence
      
      def self.included(base)
        base.extend Shuttle::Satellite::Persistence::ClassMethods
      end
      
      module ClassMethods
        
        def load(data)
          Shuttle::Satellite.new(YAML.load(data))
        end
        
        def load_file(path)
          return nil unless File.exist?(path)
          Shuttle::Satellite.new(YAML.load_file(path))
        end
        
      end
      
      def dump(io=nil)
        data = {}
        
        private_vars = %w( basedomain subdomain )
        instance_variables.each do |ivar_name|
          ivar_name = ivar_name.to_s
          ivar_name =~ /^@(.+)$/
          name = $1
          unless private_vars.include? name
            data[name] = instance_variable_get(ivar_name.to_sym)
          end
        end
        
        if io
          io.write YAML.dump(data)
        else
          YAML.dump(data)
        end
      end
      
      def dump_file(path)
        File.open(path, 'w+') { |f| dump(f) }
      end
      
    end
  end
end