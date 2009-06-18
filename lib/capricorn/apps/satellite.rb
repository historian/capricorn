Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Satellite < Thor
      desc 'list', 'show all managed satellites'
      method_options :token => :optional
      def list
        satellites = Capricorn.client(options[:token]).satellites
        satellites.size.times do |i|
          sat = satellites[i]
          puts sat.domain
          sat.engines.each do |name, options|
            puts "- #{name} #{options.inspect}"
          end
        end
      rescue => e
        p e
        puts e.backtrace
      end
      
      desc 'install DOMAIN', 'install a satellite'
      method_options :token => :optional
      def install(domain)
        Capricorn.client(options[:token]).install_satellite(domain)
      end
      
      desc 'uninstall DOMAIN', 'uninstall a satellite'
      method_options :token => :optional
      def uninstall(domain)
        Capricorn.client(options[:token]).uninstall_satellite(domain)
      end
    end
    
  end
end