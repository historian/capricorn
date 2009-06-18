

module Capricorn
  class Satellite
    class DependencyLoader
      
      attr_reader :names, :specs, :engines
      
      def self.load_for(engines)
        dependency_loader = self.new(engines)
        dependency_loader.add_dependecies!
        dependency_loader.order_by_dependecies!
        
        return dependency_loader
      end
      
      def initialize(engines)
        original_paths = [Gem.path].flatten
        Gem.refresh
        Gem.send(:set_paths, original_paths.compact.join(File::PATH_SEPARATOR))
        
        specs = engines.collect do |k,r|
          s = Gem.source_index.find_name(k, Gem::Requirement.new(r[:version] || ">= 0.0.0"))
          s.last
        end.compact
        
        @names = specs.collect { |spec| spec.name }
        @specs = specs.inject({}) { |h, spec| h[spec.name] = spec ; h }
        @engines = engines
      end
      
      def add_dependecies!
        @specs.values.each do |spec|
          add_dependecies_for spec
        end
      end
      
      def order_by_dependecies!
        @names = tsort
      end
      
      def each
        @names.each do |name|
          yield(@specs[name])
        end
      end
      
      def reverse_each
        @names.reverse.each do |name|
          yield(@specs[name])
        end
      end
      
    private
      
      include TSort
      
      def add_dependecies_for(spec)
        engine_dependencies = spec.engine_dependencies || {}
        engine_dependencies.each do |name, options|
          gems = Gem.source_index.find_name(name, [options[:version]].compact)
          next unless gem = gems.last
          next if @names.include?(name) and gem.version <= @specs[gem.name].version
          @specs[gem.name] = gem
          @names.push(gem.name)
          add_dependecies_for(gem)
        end
        @engines = @engines.merge(engine_dependencies)
      end
      
      def tsort_each_node(&block)
        @names.each(&block)
      end
      
      def tsort_each_child(node, &block)
        engine_dependencies = @specs[node].engine_dependencies || {}
        engine_dependencies.keys.each(&block)
      end
      
    end
  end
end
