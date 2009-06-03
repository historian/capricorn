Shuttle.runtime_gem('thor', Shuttle::THOR_VERSION)

module Shuttle
  module Apps # :nodoc:
    
    class Dev < Thor
      desc "create NAME", "create a new engine"
      def create(name)
        Shuttle.runtime_gem('rubigen', Shuttle::RUBIGEN_VERSION)
        
        system("rails #{name}")
        
        FileUtils.rm_r("#{name}/doc", :verbose => true)
        FileUtils.rm_r("#{name}/README", :verbose => true)
        FileUtils.rm_r("#{name}/public/javascripts", :verbose => true)
        
        require 'rubigen/scripts/generate'
        RubiGen::Base.use_application_sources!
        RubiGen::Scripts::Generate.new.run(["-f", name], :generator => 'engine')
      end
      
      desc "link", "link the current development app"
      def link
      end
    end
    
  end
end