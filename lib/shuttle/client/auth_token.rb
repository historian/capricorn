require 'yaml'

module Shuttle
  class Client
    class AuthToken
      
      def self.load(io)
        self.new YAML.load(io)
      end
      
      def self.load_file(path)
        self.new YAML.load_file(path)
      end
      
      attr_reader :target_uri
      attr_reader :verify_mode
      attr_reader :ca_certificate_data
      attr_reader :private_key_data
      attr_reader :certificate_data
      
      def initialize(options={})
        @target_uri          = options[:target_uri]
        @verify_mode         = options[:verify_mode]
        @ca_certificate_data = options[:ca_certificate_data]
        @private_key_data    = options[:private_key_data]
        @certificate_data    = options[:certificate_data]
      end
      
      def ca_certificate
        @ca_certificate ||= OpenSSL::X509::Certificate.new(@ca_certificate_data)
      end
      
      def certificate
        @certificate ||= OpenSSL::X509::Certificate.new(@certificate_data)
      end
      
      def private_key
        @private_key ||= OpenSSL::PKey::RSA.new(@private_key_data)
      end
      
      def connect
        use_ssl, uri = Shuttle::Client.parse_uri(self.target_uri)
        if use_ssl
          DRb.start_service nil, nil, self.options_for_drb
        else
          DRb.start_service
        end
        DRbObject.new nil, uri
      end
      
      def options_for_drb
        @options_for_drb ||= {
          :SSLVerifyMode => self.verify_mode,
          :SSLCACertificate => self.ca_certificate,
          :SSLPrivateKey => self.private_key,
          :SSLCertificate => self.certificate
        }
      end
      
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
      
      def dump_file(path)
        File.open(path, 'w+') { |f| dump(f) }
      end
      
    end
  end
end