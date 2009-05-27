
module Shuttle
  class App < Thor
    
    desc "start", 'start the server'
    def start
      Shuttle::Server.start
    end
    
    desc "stop", 'stop the server'
    def stop
      Shuttle::Server.stop
    end
    
    desc 'env', 'show the environment'
    def env
      
    end
    
    desc 'domains', 'show all managed domains'
    def domains
      
    end
    
  end
end