require 'thor'

module Shuttle
  module Apps # :nodoc:
    
    class Engines < Thor
      desc 'install DOMAIN NAME', 'install an engine'
      method_options :version => :required, :lib => :optional, :source => :optional, :token => :optional
      def install(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Shuttle.client(options[:token]).install_engine(domain, name, desc)
      end
      
      desc 'update DOMAIN NAME', 'update an engine'
      method_options :version => :required, :lib => :optional, :source => :optional, :token => :optional
      def update(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Shuttle.client(options[:token]).update_engine(domain, name, desc)
      end
      
      desc 'uninstall DOMAIN NAME', 'uninstall an engine'
      method_options :token => :optional
      def uninstall(domain, name)
        Shuttle.client(options[:token]).uninstall_engine(domain, name)
      end
    end
    
  end
end