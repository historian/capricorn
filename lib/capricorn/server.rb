
module Capricorn
  
  # the Server is supposed to be a simple DRb interface to the System.
  class Server < Capricorn::Daemon::Base
    include DRbUndumped
    
    autoload :Proxy,    File.dirname(__FILE__)+'/server/proxy'
    autoload :Daemon,   File.dirname(__FILE__)+'/server/daemon'
    autoload :Security, File.dirname(__FILE__)+'/server/security'
    
    include Capricorn::Server::Daemon
    include Capricorn::Server::Security
    
    def self.shared
      @shared ||= new
    end
    
    def self.proxy
      @proxy ||= Capricorn::Server::Proxy.new(self.shared)
    end
    
    def stop_server
      Capricorn.log "stopping the server..."
      $exitstatus = Capricorn::STOP_STATUS
      DRb.stop_service
    end
    
    def restart_server
      Capricorn.log "restarting the server..."
      $exitstatus = Capricorn::RESTART_STATUS
      DRb.stop_service
    end
    
    def reload_server
      Capricorn.log "reloading the server..."
      $exitstatus = Capricorn::RELOAD_STATUS
      DRb.stop_service
    end
    
    def server_version
      Capricorn.version
    end
    
    def update_server
      if Capricorn.system.gem_update('capricorn', :user => 'root')
        reload_server
      end
    end
    
    def update_gems
      Capricorn.system.gem_update(:all)
    end
    
    def install_satellite(domain, immediate)
      Capricorn.system.install_satellite(domain, immediate)
    end
    
    def relink_satellite(domain, immediate)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.relink_satellite(satellite, immediate)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def update_satellite(domain, immediate)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.update_satellite(satellite, immediate)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def uninstall_satellite(domain, immediate)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.uninstall_satellite(satellite, immediate)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def make_development_satellite(domain, name)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.make_development_satellite(satellite, name)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def install_engine(domain, name, options, immediate)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.install_engine(satellite, name, options, immediate)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def update_engine(domain, name, options, immediate)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.update_engine(satellite, name, options, immediate)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def uninstall_engine(domain, name, immediate)
      satellite = Capricorn.system.find_satellite(domain)
      if satellite
        Capricorn.system.uninstall_engine(satellite, name, immediate)
      else
        Capricorn.log "Satellite not found (#{domain})"
      end
    end
    
    def satellites
      Capricorn.system.satellites
    end
    
    def queued_jobs
      queued_jobs = []
      Capricorn.system.queue.each do |job, canceled, immediated|
        job_delay = job.delay
        queued_jobs.push([job.id, job.name, canceled, immediated, job.running?, job.waiting?, job_delay])
      end
      queued_jobs
    end
    
    def cancel_job(id)
      Capricorn.system.queue.cancel(id)
    end
    
    def immediate_job(id)
      Capricorn.system.queue.immediate(id)
    end
    
  end
end
