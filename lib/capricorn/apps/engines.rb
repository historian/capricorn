Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Engines < Thor
      namespace :engines
      
      class_option :token,
        :desc => 'Name or path of a token.',
        :banner => 'name',
        :type => :string,
        :required => false,
        :aliases => %w( -t )
      
      class_option :immediate,
        :desc => 'Execute this task immediately.',
        :type => :boolean,
        :default => false,
        :aliases => %w( -i )
      
      class_option :version,
        :desc => 'Version of the engine to use (ignored by uninstall task).',
        :type => :string,
        :required => true,
        :aliases => %w( -v )
      
      class_option :lib,
        :desc => 'Lib path of the engine (ignored by uninstall task).',
        :type => :string,
        :required => false,
        :aliases => %w( -l )
      
      class_option :source,
        :desc => 'Source of the engine (ignored by uninstall task).',
        :type => :string,
        :required => false,
        :aliases => %w( -s )
      
      desc 'install DOMAIN NAME', 'install an engine'
      def install(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Capricorn.client(options[:token]).install_engine(domain, name, desc, options[:immediate])
      end
      
      desc 'update DOMAIN NAME', 'update an engine'
      def update(domain, name)
        desc = { :version => options[:version] }
        desc[:lib]    = options[:lib]    if options[:lib]
        desc[:source] = options[:source] if options[:source]
        Capricorn.client(options[:token]).update_engine(domain, name, desc, options[:immediate])
      end
      
      desc 'uninstall DOMAIN NAME', 'uninstall an engine'
      def uninstall(domain, name)
        Capricorn.client(options[:token]).uninstall_engine(domain, name, options[:immediate])
      end
    end
    
  end
end