
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
      
      def update_all_engines
        @engines.each do |gem_name,options|
          Capricorn.system.gem_update(gem_name)
          
          dep = Gem::Dependency.new(gem_name.to_s, Gem::Requirement.default)
          specs = Gem::SourceIndex.from_installed_gems.search(dep)
          
          return false if specs.empty?
          
          specs.sort! do |a,b|
            b.version <=> a.version
          end
          
          spec_version = specs.first.version.to_s
          options[:version] = spec_version
        end
        
        return true
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