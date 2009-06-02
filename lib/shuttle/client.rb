require 'drb'
require 'drb/ssl'
require 'uri'

module Shuttle
  class Client
    
    autoload :AuthToken, File.dirname(__FILE__)+'/client/auth_token'
    
    # return a DRb uri for the given Shuttle uri.
    def self.parse_uri(uri)
      uri     = URI.parse(uri)
      use_ssl = (uri.scheme == 'ssl+shuttle')
      uri.scheme = 'druby'
      return use_ssl, uri.to_s
    end
    
    # return an potentialy initialize the client to the given token.
    def self.current(token=nil)
      @client = connect(token) unless @client
      @client
    end
    
    # connect to the server referenced by the given token.
    def self.connect(token=nil)
      token ||= 'core.token'
      
      [Shuttle::DEFAULT_ROOT_SYSTEM_DIR,
       Shuttle::DEFAULT_USER_SYSTEM_DIR,
       File.join(Shuttle::DEFAULT_USER_SYSTEM_DIR, 'tokens'),
       '.'].each do |path|
        path = File.expand_path(File.join(path, token))
        if File.file? path
          token = path
          break
        end
      end
      
      unless File.file? token
        raise "Unable to read the token at: #{token}"
      end
      
      token = Shuttle::Client::AuthToken.load_file(token) if String === token
      token.connect if token
    end
    
  end
end