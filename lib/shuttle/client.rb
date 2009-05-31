require 'drb'
require 'drb/ssl'
require 'uri'

module Shuttle
  class Client
    
    autoload :AuthToken, File.dirname(__FILE__)+'/client/auth_token'
    
    def self.parse_uri(uri)
      uri     = URI.parse(uri)
      use_ssl = (uri.scheme == 'ssl+shuttle')
      uri.scheme = 'druby'
      return use_ssl, uri.to_s
    end
    
    def self.current(token='core.token')
      [Shuttle::DEFAULT_ROOT_SYSTEM_DIR,
       Shuttle::DEFAULT_USER_SYSTEM_DIR,
       File.join(Shuttle::DEFAULT_USER_SYSTEM_DIR, 'tokens')].each do |path|
        path = File.join(path, token)
        if File.file? path
          token = path
          break
        end
      end
      
      @client = connect(token) unless @client
      @client
    end
    
    def self.connect(token)
      token = Shuttle::Client::AuthToken.load_file(token) if String === token
      token.connect
    end
    
  end
end