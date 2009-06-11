
module Capricorn
  class Server
    module Security # :nodoc:
      
      def self.included(base)
        base.extend Capricorn::Server::Security::ClassMethods
      end
      
      module ClassMethods
        
        def options_for_server
          
          config = {}
          if Capricorn.system.use_ssl?
            install_quick_cert!
            dump_quick_cert_config!
            run_quick_cert!
            
            keypair = Capricorn.system.path('quick_cert', 'capricorn', 'capricorn_keypair.pem')
            cert    = Capricorn.system.path('quick_cert', 'capricorn', 'cert_capricorn.pem')
            
            config = {
              :SSLPrivateKey  => OpenSSL::PKey::RSA.new(File.read(keypair)),
              :SSLCertificate => OpenSSL::X509::Certificate.new(File.read(cert)),
              :SSLVerifyMode  => OpenSSL::SSL::VERIFY_PEER | OpenSSL::SSL::VERIFY_FAIL_IF_NO_PEER_CERT,
              :SSLCACertificateFile => Capricorn.system.path('quick_cert', 'CA', 'cacert.pem')
            }
          end
          
          config
        end
        
        def install_quick_cert!
          unless Capricorn.system.find_bin('QuickCert')
            FileUtils.mkdir_p('/tmp/quick_cert')
            File.chmod(0700, '/tmp/quick_cert')
            
            download_quick_cert!
            build_quick_cert!
            
            FileUtils.rm_rf('/tmp/quick_cert')
          end
        end
        
        def download_quick_cert!
          Dir.chdir('/tmp/quick_cert') do
            Capricorn.system.run "curl -O #{Capricorn::QUICK_CERT}"
            Capricorn.system.run "tar xzf QuickCert-1.0.2.tar.gz"
          end
        end
        
        def build_quick_cert!
          Dir.chdir('/tmp/quick_cert/QuickCert-1.0.2') do
            Capricorn.system.run "#{Capricorn.system.ruby_path} ./setup.rb config"
            Capricorn.system.run "#{Capricorn.system.ruby_path} ./setup.rb setup"
            Capricorn.system.run "#{Capricorn.system.ruby_path} ./setup.rb install"
          end
        end
        
        def dump_quick_cert_config!
          return if File.file? Capricorn.system.path('quick_cert', 'qc_config')
          
          config = %{
full_hostname = `hostname`.strip
domainname = full_hostname.split('.')[1..-1].join('.')
hostname = full_hostname.split('.')[0]

CA[:hostname] = hostname
CA[:domainname] = domainname
CA[:CA_dir] = File.join Dir.pwd, "CA"
CA[:password] = '#{rand(100_000)}'

CERTS << {
:type => 'server',
:hostname => 'capricorn',
# :password => '#{rand(100_000)}',
}

CERTS << {
:type => 'client',
:user => 'core',
:email => 'core@mrhenry.be',
}
}
          FileUtils.mkdir_p(Capricorn.system.path('quick_cert'))
          File.chmod(0700, Capricorn.system.path('quick_cert'))
          File.open(Capricorn.system.path('quick_cert', 'qc_config'), 'w+') { |f| f.write config }
        end
        
        def run_quick_cert!
          return if File.directory? Capricorn.system.path('quick_cert', 'CA')
          
          Dir.chdir(Capricorn.system.path('quick_cert')) do
            Capricorn.system.run Capricorn.system.find_bin('QuickCert')
          end
        end
        
        def make_client_cert_public!
          token = nil
          if Capricorn.system.use_ssl?
            token = Capricorn::Client::AuthToken.new(
              :target_uri => self.construct_uri(DRb.uri),
              :verify_mode => OpenSSL::SSL::VERIFY_PEER,
              :private_key_data => File.read(Capricorn.system.path('quick_cert', 'core', 'core_keypair.pem')),
              :certificate_data => File.read(Capricorn.system.path('quick_cert', 'core', 'cert_core.pem')),
              :ca_certificate_data => File.read(Capricorn.system.path('quick_cert', 'CA', 'cacert.pem'))
            )
          else
            token = Capricorn::Client::AuthToken.new(
              :target_uri => self.construct_uri(DRb.uri))
          end
          token.dump_file(Capricorn.system.path('core.token'))
          FileUtils.chmod_R(0775, Capricorn.system.path('core.token'))
        end
        
      end
    end
  end
end
