Shuttle.runtime_gem('thor', Shuttle::THOR_VERSION)

module Shuttle
  module Apps # :nodoc:
    
    class Satellite < Thor
      desc 'list', 'show all managed satellites'
      method_options :token => :optional
      def list
        Shuttle.client(options[:token]).satellites.each do |sat|
          puts sat.domain
          sat.engines.each do |name, options|
            puts "- #{name} #{options.inspect}"
          end
        end
      end
      
      desc 'install DOMAIN', 'install a satellite'
      method_options :token => :optional
      def install(domain)
        Shuttle.client(options[:token]).install_satellite(domain)
      end
      
      desc 'uninstall DOMAIN', 'uninstall a satellite'
      method_options :token => :optional
      def uninstall(domain)
        Shuttle.client(options[:token]).uninstall_satellite(domain)
      end
    end
    
  end
end