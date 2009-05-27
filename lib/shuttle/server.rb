
module Shuttle
  class Server
    
    def self.stop
      
    end
    
    def self.start
      Daemons.daemonize
    end
    
    
    
  end
end