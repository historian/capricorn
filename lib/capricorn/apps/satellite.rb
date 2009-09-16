Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Satellite < Thor
      desc 'list [DOMAIN]', 'show info for managed satellites'
      method_options :token => :optional
      def list(domain=nil)
        satellites = Capricorn.client(options[:token]).satellites.dup
        
        names_by_index = []
        satellites.size.times do |i|
          sat = satellites[i]
          names_by_index.push([sat.domain, i])
        end
        names_by_index.sort! { |a,b| a.first <=> b.first }
        
        names_by_index.each do |(domain_, i)|
          sat = satellites[i]
          if domain.nil? or sat.domain.include?(domain)
            puts [
              sat.domain,
              (sat.development ? ['development:', sat.module_name] : [])
            ].flatten.join(' ')
            sat.engines.each do |name, options|
              puts "- #{name} #{options.inspect}"
            end
          end
        end
      rescue => e
        p e
        puts e.backtrace
      end
      
      desc 'install DOMAIN', 'install a satellite'
      method_options :token => :optional, :immediate => :boolean
      def install(domain)
        Capricorn.client(options[:token]).install_satellite(domain, options[:immediate])
      end
      
      desc 'uninstall DOMAIN', 'uninstall a satellite'
      method_options :token => :optional, :immediate => :boolean
      def uninstall(domain)
        Capricorn.client(options[:token]).uninstall_satellite(domain, options[:immediate])
      end
      
      desc 'relink DOMAIN', 'relink a satellite'
      method_options :token => :optional, :immediate => :boolean
      def relink(domain)
        Capricorn.client(options[:token]).relink_satellite(domain, options[:immediate])
      end
      
      desc 'update DOMAIN', 'update the installed engines of a satellite'
      method_options :token => :optional, :immediate => :boolean
      def update(domain)
        Capricorn.client(options[:token]).update_satellite(domain, options[:immediate])
      end
      
      # desc 'upgrade DOMAIN', 'upgrade/rebuild the rails app of a satellite'
      # method_options :token => :optional, :immediate => :boolean
      # def upgrade(domain)
      #   Capricorn.client(options[:token]).upgrade_satellite(domain)
      # end
    end
    
  end
end