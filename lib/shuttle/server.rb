
module Shuttle
  class Server < SimpleDaemon::Base
    
    def self.construct_uri(uri)
      uri        = URI.parse(uri)
      uri.scheme = ( Shuttle.system.use_ssl? ? 'ssl+shuttle' : 'shuttle')
      uri.to_s
    end
    
    def self.shared
      @shared ||= new
    end
    
    def self.stop
      Shuttle.client.stop_server
    end
    
    def self.start
      start_with_failsafe
    end
    
    def self.start_with_failsafe
      stop_server = false
      wait_before_start = 0
      retries = 0
      until stop_server
        if wait_before_start > 0
          sleep(wait_before_start)
          wait_before_start = 0
        end
        
        pid = Process.fork { self.run_server }
        Process.waitpid(pid, 0)
        case $?.exitstatus
        when Shuttle::STOP_STATUS
          stop_server = true
        when Shuttle::RESTART_STATUS
          wait_before_start = 2
        when Shuttle::RELOAD_STATUS
          stop_server = true
          Shuttle.system.run(%{sleep 2 ; shuttle #{ORIGINAL_ARGV.join(' ')}})
        else
          retries += 1
          stop_server = true if retries >= 3
        end
      end
      
      path = Shuttle.config.path('Server.pid')
      File.unlink(path) if File.file?(path)
    end
    
    def self.run_server
      Dir.chdir(Shuttle.config.root)
      
      config = {}
      if Shuttle.system.use_ssl?
        install_quick_cert!
        dump_quick_cert_config!
        run_quick_cert!
        make_client_cert_public!
        
        keypair = Shuttle.config.path('quick_cert', 'shuttle', 'shuttle_keypair.pem')
        cert    = Shuttle.config.path('quick_cert', 'shuttle', 'cert_shuttle.pem')
        
        config = {
          :SSLPrivateKey  => OpenSSL::PKey::RSA.new(File.read(keypair)),
          :SSLCertificate => OpenSSL::X509::Certificate.new(File.read(cert)),
          :SSLVerifyMode  => OpenSSL::SSL::VERIFY_PEER | OpenSSL::SSL::VERIFY_FAIL_IF_NO_PEER_CERT,
          :SSLCACertificateFile => Shuttle.config.path('quick_cert', 'CA', 'cacert.pem')
        }
      end
      
      Shuttle.log "Server started"
      DRb.start_service 'druby://localhost:5000', self.shared, config
      Shuttle.log "listening at #{self.construct_uri(DRb.uri)}"
      
      at_exit do
        unless $task_child
          Shuttle.log "Server stopped"
        end
      end
      
      DRb.thread.join
    end
    
    def stop_server
      Shuttle.log "stopping the server..."
      Thread.new { sleep(1) and exit(Shuttle::STOP_STATUS) }
    end
    
    def restart_server
      Shuttle.log "restarting the server..."
      Thread.new { sleep(1) and exit(Shuttle::RESTART_STATUS) }
    end
    
    def reload_server
      Shuttle.log "reloading the server..."
      Thread.new { sleep(1) and exit(Shuttle::RELOAD_STATUS) }
    end
    
    def server_version
      Shuttle.version
    end
    
    def update_server
      Shuttle.system.gem_update('engine_manager')
      if Shuttle.system.gem_update('shuttle', :user => 'root')
        reload_server
      end
    end
    
    def install_satellite(domain)
      Shuttle::Satellite.add_satellite(Shuttle::Satellite.new(domain))
      run_installer!
      run_linker!
      Shuttle::Satellite.save!
    end
    
    def uninstall_satellite(domain)
      Shuttle::Satellite.find(domain).needs_uninstall!
      run_uninstaller!
      Shuttle::Satellite.save!
    end
    
    def install_engine(domain, name, options={})
      satellite = Shuttle::Satellite.find(domain)
      if satellite
        Shuttle.system.ensure_precense_of_gem(name, options)
        satellite.add_engine(name, options)
        satellite.needs_link!
        run_linker!
        Shuttle::Satellite.save!
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def update_engine(domain, name, options={})
      satellite = Shuttle::Satellite.find(domain)
      if satellite
        Shuttle.system.ensure_precense_of_gem(name, options)
        satellite.update_engine(name, options)
        satellite.needs_link!
        run_linker!
        Shuttle::Satellite.save!
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def uninstall_engine(domain, name)
      satellite = Shuttle::Satellite.find(domain)
      if satellite
        satellite.remove_engine(name)
        satellite.needs_link!
        run_linker!
        Shuttle::Satellite.save!
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def satellites
      Shuttle::Satellite.all
    end
    
    def generate_certs
      install_quick_cert!
      dump_quick_cert_config!
      run_quick_cert!
    end
    
    private
    
    def self.install_quick_cert!
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
    
    def self.dump_quick_cert_config!
      return if File.file? Shuttle.config.path('quick_cert', 'qc_config')
      
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
      FileUtils.mkdir_p(Shuttle.config.path('quick_cert'))
      File.chmod(0700, Shuttle.config.path('quick_cert'))
      File.open(Shuttle.config.path('quick_cert', 'qc_config'), 'w+') { |f| f.write config }
    end
    
    def self.run_quick_cert!
      return if File.directory? Shuttle.config.path('quick_cert', 'CA')
      
      Dir.chdir(Shuttle.config.path('quick_cert')) do
        Shuttle.system.run Shuttle.system.find_bin('QuickCert')
      end
    end
    
    def self.make_client_cert_public!
      return if File.directory? Shuttle.config.path('client_cert')
      
      FileUtils.mkdir_p(Shuttle.config.path('client_cert'))
      FileUtils.mkdir_p(Shuttle.config.path('client_cert', 'CA'))
      FileUtils.mkdir_p(Shuttle.config.path('client_cert', 'core'))
      FileUtils.cp(Shuttle.config.path('quick_cert',  'core', 'core_keypair.pem'),
                   Shuttle.config.path('client_cert', 'core', 'core_keypair.pem'))
      FileUtils.cp(Shuttle.config.path('quick_cert',  'core', 'cert_core.pem'),
                   Shuttle.config.path('client_cert', 'core', 'cert_core.pem'))
      FileUtils.cp(Shuttle.config.path('quick_cert',  'CA',   'cacert.pem'),
                   Shuttle.config.path('client_cert', 'CA',   'cacert.pem'))
      FileUtils.chmod_R(0775, Shuttle.config.path('client_cert'))
    end
    
    def run_installer!
      Shuttle::Satellite.all.each do |satellite|
        if satellite.needs_install?
          Shuttle.system.install(satellite)
          satellite.needs_install! false
          satellite.needs_link!
        end
      end
    end
    
    def run_uninstaller!
      Shuttle::Satellite.all.each do |satellite|
        if satellite.needs_uninstall?
          Shuttle.system.uninstall(satellite)
          satellite.needs_uninstall! false
          Shuttle::Satellite.remove_satellite(satellite)
        end
      end
    end
    
    def run_linker!
      Shuttle::Satellite.all.each do |satellite|
        if satellite.needs_link?
          Shuttle.system.link(satellite)
          satellite.needs_link! false
        end
      end
    end
    
  end
end
