
module Shuttle
  class Client
    
    def self.parse_uri(uri='ssl+shuttle://localhost:5000')
      uri     = URI.parse(uri)
      use_ssl = (uri.scheme == 'ssl+shuttle')
      uri.scheme = 'druby'
      return use_ssl, uri.to_s
    end
    
    def self.shared
      unless @client
        
        use_ssl, uri = self.parse_uri
        
        config = {}
        
        if use_ssl
          keypair = Shuttle.config.path('client_cert', 'core', 'core_keypair.pem')
          cert    = Shuttle.config.path('client_cert', 'core', 'cert_core.pem')
          
          config = {
            :SSLVerifyMode => OpenSSL::SSL::VERIFY_PEER,
            :SSLCACertificateFile => Shuttle.config.path('client_cert', 'CA', 'cacert.pem'),
            :SSLPrivateKey => OpenSSL::PKey::RSA.new(File.read(keypair)),
            :SSLCertificate => OpenSSL::X509::Certificate.new(File.read(cert))
          }
        end
        
        DRb.start_service nil, nil, config
        @client = DRbObject.new nil, uri
        
      end
      @client
    end
    
  end
end