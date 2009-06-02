
module Shuttle
  class Satellite
    
    autoload :Actions,          File.dirname(__FILE__)+'/satellite/actions'
    autoload :Persistence,      File.dirname(__FILE__)+'/satellite/persistence'
    autoload :DependencyLoader, File.dirname(__FILE__)+'/satellite/dependency_loader'
    
    include Shuttle::Satellite::Actions
    include Shuttle::Satellite::Persistence
    
    attr_reader :domain, :engines
    
    def initialize(domain)
      if Hash === domain
        domain.each do |name, value|
          instance_variable_set("@#{name}".to_sym, value)
        end
      else
        @domain = domain
        @engines = {}
      end
      @domain.gsub! /^www\./, ''
    end
    
    def basedomain
      unless @basedomain
        parts = self.domain.split('.')
        parts = parts[-2..-1]
        @basedomain = parts.join('.')
      end
      @basedomain
    end
    
    def subdomain
      unless @subdomain
        parts = self.domain.split('.')
        parts = parts[0..-3]
        @subdomain = parts.join('.')
      end
      @subdomain unless @subdomain == ''
    end
    
    def subdomain?
      !self.subdomain.nil?
    end
    
  end
end