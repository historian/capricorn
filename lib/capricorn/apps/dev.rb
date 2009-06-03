Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Dev < Thor
      desc "activate DOMAIN", "turn an existing satelite into a development satelite"
      def activate(domain)
        Capricorn.runtime_gem('rubigen', Capricorn::RUBIGEN_VERSION)
        Capricorn.client.make_development_satellite(domain)
      end
    end
    
  end
end