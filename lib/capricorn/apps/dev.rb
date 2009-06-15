Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Dev < Thor
      desc "activate DOMAIN NAME", "turn an existing satelite into a development satelite"
      def activate(domain, name)
        Capricorn.runtime_gem('rubigen', Capricorn::RUBIGEN_VERSION)
        Capricorn.client.make_development_satellite(domain, name)
      end
    end
    
  end
end