require 'yaml'

module Capricorn
  class Client
    class AuthToken
      
      # load a token from the passed IO.
      def self.load(io)
        self.new YAML.load(io)
      end
      
      # load a token from a file referenced by the given +path+.
      def self.load_file(path)
        self.new YAML.load_file(path)
      end
      
      # the uri at which the capricorn server can be accessed.
      attr_reader :target_uri
      # the SSL verification mode used by the capricorn server
      attr_reader :verify_mode
      # the optional CA certificate used by the capricorn server
      attr_reader :ca_certificate_data
      # the private key used by the client
      attr_reader :private_key_data
      # the certificate used by the client
      attr_reader :certificate_data
      
      # create a new token from the given options
      #
      # +:target_uri+, +:verify_mode+, +:ca_certificate_data+, +:private_key_data+, +:certificate_data+
      def initialize(options={})
        @target_uri          = options[:target_uri]
        @verify_mode         = options[:verify_mode]
        @ca_certificate_data = options[:ca_certificate_data]
        @private_key_data    = options[:private_key_data]
        @certificate_data    = options[:certificate_data]
      end
      
      # get the parsed and initialized OpenSSL::X509::Certificate
      def ca_certificate
        @ca_certificate ||= OpenSSL::X509::Certificate.new(@ca_certificate_data)
      end
      
      # get the parsed and initialized OpenSSL::X509::Certificate
      def certificate
        @certificate ||= OpenSSL::X509::Certificate.new(@certificate_data)
      end
      
      # get the parsed and initialized OpenSSL::PKey::RSA
      def private_key
        @private_key ||= OpenSSL::PKey::RSA.new(@private_key_data)
      end
      
      # connect to the server and return the server handle.
      def connect
        use_ssl, uri = Capricorn::Client.parse_uri(self.target_uri)
        if use_ssl
          DRb.start_service nil, nil, self.options_for_drb
        else
          DRb.start_service
        end
        DRbObject.new nil, uri
      end
      
      # return options for use with DRb
      def options_for_drb
        @options_for_drb ||= {
          :SSLVerifyMode => self.verify_mode,
          :SSLCACertificate => self.ca_certificate,
          :SSLPrivateKey => self.private_key,
          :SSLCertificate => self.certificate
        }
      end
      
      # dump this token to the given IO or return the content as a String
      def dump(io=nil)
        data = {
          :target_uri => self.target_uri,
          :verify_mode => self.verify_mode,
          :ca_certificate_data => self.ca_certificate_data,
          :private_key_data => self.private_key_data,
          :certificate_data => self.certificate_data
        }
        if io
          io.write YAML.dump(data)
        else
          YAML.dump(data)
        end
      end
      
      # dump this token to a file at the given +path+.
      def dump_file(path)
        File.open(path, 'w+') { |f| dump(f) }
      end
      
    end
  end
end