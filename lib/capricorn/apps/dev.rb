Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

module Capricorn
  module Apps # :nodoc:
    
    class Dev < Thor
      desc "create NAME", "create a new engine"
      def create(name)
        Capricorn.runtime_gem('rubigen', Capricorn::RUBIGEN_VERSION)
        
        system("rails #{name}")
        
        FileUtils.rm_r("#{name}/doc", :verbose => true)
        FileUtils.rm_r("#{name}/README", :verbose => true)
        FileUtils.rm_r("#{name}/public/javascripts", :verbose => true)
        
        require 'rubigen/scripts/generate'
        RubiGen::Base.use_application_sources!
        RubiGen::Scripts::Generate.new.run(["-f", name], :generator => 'engine')
      end
    end
    
  end
end