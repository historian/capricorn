
module Shuttle
  class Client
    
    def self.shared
      unless @client
        uri_file = Shuttle.config.path('Server.uri')
        if File.file? uri_file
          DRb.start_service
          @client = DRbObject.new nil, File.read(uri_file).strip
        end
      end
      @client
    end
    
  end
end