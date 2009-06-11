require 'rubygems/specification'

module Gem # :nodoc:
  class Specification
    attribute :engine_dependencies, {}
    
    def add_engine_dependency(name, options={})
      name = name.to_s
      add_runtime_dependency(name, *[options[:version]].compact)
      @engine_dependencies ||= {}
      @engine_dependencies[name] = options
    end
    
    alias_method :ruby_code_without_engines, :ruby_code # :nodoc:
    def ruby_code(obj) # :nodoc:
      return obj.inspect if Hash === obj
      return ruby_code_without_engines(obj)
    end
    
    alias_method :to_ruby_without_engines, :to_ruby # :nodoc:
    def to_ruby # :nodoc:
      code = to_ruby_without_engines
      code.gsub!(/s\.engine_dependencies\s+[=]([^\n]+)/, 's.instance_variable_set(:@engine_dependencies,\1)')
      code
    end
  end
end