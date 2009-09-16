Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Engines < Thor
      desc 'install DOMAIN NAME', 'install an engine'
      method_options :version => :required, :lib => :optional, :source => :optional, :token => :optional, :immediate => :boolean
      def install(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Capricorn.client(options[:token]).install_engine(domain, name, desc, options[:immediate])
      end
      
      desc 'update DOMAIN NAME', 'update an engine'
      method_options :version => :required, :lib => :optional, :source => :optional, :token => :optional, :immediate => :boolean
      def update(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Capricorn.client(options[:token]).update_engine(domain, name, desc, options[:immediate])
      end
      
      desc 'uninstall DOMAIN NAME', 'uninstall an engine'
      method_options :token => :optional, :immediate => :boolean
      def uninstall(domain, name)
        Capricorn.client(options[:token]).uninstall_engine(domain, name, options[:immediate])
      end
    end
    
  end
end