
module Shuttle
  class Server < SimpleDaemon::Base
    
    attr_accessor :uri
    
    def self.shared
      @shared ||= new
    end
    
    def self.stop
      clean_working_files
    end
    
    def self.start
      Dir.chdir(Shuttle.config.root)
      DRb.start_service nil, self.shared
      self.shared.uri = DRb.uri
      puts "listening at #{DRb.uri}"
      
      dump_uri
      
      DRb.thread.join
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
        satellite.add_engine(name, options)
        satellite.needs_link!
        run_linker!
        Shuttle::Satellite.save!
      else
        puts "Satellite not found (#{domain})"
      end
    end
    
    def update_engine(domain, name, options={})
      satellite = Shuttle::Satellite.find(domain)
      if satellite
        satellite.update_engine(name, options)
        satellite.needs_link!
        run_linker!
        Shuttle::Satellite.save!
      else
        puts "Satellite not found (#{domain})"
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
        puts "Satellite not found (#{domain})"
      end
    end
    
    def satellites
      Shuttle::Satellite.all
    end
    
    private
    
    def self.dump_uri
      path = Shuttle.config.path('Server.uri')
      File.open(path, 'w+') { |f| f.write self.shared.uri }
      File.chmod(0664, path)
    end
    
    def self.clean_working_files
      path = Shuttle.config.path('Server.uri')
      File.unlink(path) if File.file?(path)
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
