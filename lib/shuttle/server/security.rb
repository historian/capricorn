
module Shuttle
  class Server
    module Security # :nodoc:
      
      def self.included(base)
        base.extend Shuttle::Server::Security::ClassMethods
      end
      
      module ClassMethods
        
        def options_for_server
          
          config = {}
          if Shuttle.system.use_ssl?
            install_quick_cert!
            dump_quick_cert_config!
            run_quick_cert!
            
            keypair = Shuttle.system.path('quick_cert', 'shuttle', 'shuttle_keypair.pem')
            cert    = Shuttle.system.path('quick_cert', 'shuttle', 'cert_shuttle.pem')
            
            config = {
              :SSLPrivateKey  => OpenSSL::PKey::RSA.new(File.read(keypair)),
              :SSLCertificate => OpenSSL::X509::Certificate.new(File.read(cert)),
              :SSLVerifyMode  => OpenSSL::SSL::VERIFY_PEER | OpenSSL::SSL::VERIFY_FAIL_IF_NO_PEER_CERT,
              :SSLCACertificateFile => Shuttle.system.path('quick_cert', 'CA', 'cacert.pem')
            }
          end
          
          config
        end
        
        def install_quick_cert!
          unless Shuttle.system.find_bin('QuickCert')
            FileUtils.mkdir_p('/tmp/quick_cert')
            File.chmod(0700, '/tmp/quick_cert')
            
            Dir.chdir('/tmp/quick_cert') do
              Shuttle.system.run "curl -O #{Shuttle::QUICK_CERT}"
              Shuttle.system.run "tar xzf QuickCert-1.0.2.tar.gz"
              
              Dir.chdir('/tmp/quick_cert/QuickCert-1.0.2') do
                Shuttle.system.run "#{Shuttle.system.ruby_path} ./setup.rb config"
                Shuttle.system.run "#{Shuttle.system.ruby_path} ./setup.rb setup"
                Shuttle.system.run "#{Shuttle.system.ruby_path} ./setup.rb install"
              end
            end
            
            FileUtils.rm_rf('/tmp/quick_cert')
          end
        end
        
        def dump_quick_cert_config!
          return if File.file? Shuttle.system.path('quick_cert', 'qc_config')
          
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
:hostname => 'shuttle',
# :password => '#{rand(100_000)}',
}

CERTS << {
:type => 'client',
:user => 'core',
:email => 'core@mrhenry.be',
}
}
          FileUtils.mkdir_p(Shuttle.system.path('quick_cert'))
          File.chmod(0700, Shuttle.system.path('quick_cert'))
          File.open(Shuttle.system.path('quick_cert', 'qc_config'), 'w+') { |f| f.write config }
        end
        
        def run_quick_cert!
          return if File.directory? Shuttle.system.path('quick_cert', 'CA')
          
          Dir.chdir(Shuttle.system.path('quick_cert')) do
            Shuttle.system.run Shuttle.system.find_bin('QuickCert')
          end
        end
        
        def make_client_cert_public!
          token = nil
          if Shuttle.system.use_ssl?
            token = Shuttle::Client::AuthToken.new(
              :target_uri => self.construct_uri(DRb.uri),
              :verify_mode => OpenSSL::SSL::VERIFY_PEER,
              :private_key_data => File.read(Shuttle.system.path('quick_cert', 'core', 'core_keypair.pem')),
              :certificate_data => File.read(Shuttle.system.path('quick_cert', 'core', 'cert_core.pem')),
              :ca_certificate_data => File.read(Shuttle.system.path('quick_cert', 'CA', 'cacert.pem'))
            )
          else
            token = Shuttle::Client::AuthToken.new(
              :target_uri => self.construct_uri(DRb.uri))
          end
          token.dump_file(Shuttle.system.path('core.token'))
          FileUtils.chmod_R(0775, Shuttle.system.path('core.token'))
        end
        
      end
    end
  end
end
