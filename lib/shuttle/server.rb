require 'drb'
require 'drb/ssl'
require 'simple-daemon'

module Shuttle
  class Server < SimpleDaemon::Base
    include DRbUndumped
    
    autoload :Proxy,    File.dirname(__FILE__)+'/server/proxy'
    autoload :Daemon,   File.dirname(__FILE__)+'/server/daemon'
    autoload :Security, File.dirname(__FILE__)+'/server/security'
    
    include Shuttle::Server::Daemon
    include Shuttle::Server::Security
    
    def self.shared
      @shared ||= new
    end
    
    def self.proxy
      @proxy ||= Shuttle::Server::Proxy.new(self.shared)
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
      if Shuttle.system.gem_update('shuttle', :user => 'root')
        reload_server
      end
    end
    
    def install_satellite(domain)
      Shuttle.system.install_satellite(domain)
    end
    
    def uninstall_satellite(domain)
      satellite = Shuttle.system.find_satellite(domain)
      if satellite
        Shuttle.system.uninstall_satellite(satellite)
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def install_engine(domain, name, options={})
      satellite = Shuttle.system.find_satellite(domain)
      if satellite
        Shuttle.system.install_engine(satellite, name, options)
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def update_engine(domain, name, options={})
      satellite = Shuttle.system.find_satellite(domain)
      if satellite
        Shuttle.system.update_engine(satellite, name, options)
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def uninstall_engine(domain, name)
      satellite = Shuttle.system.find_satellite(domain)
      if satellite
        Shuttle.system.uninstall_engine(satellite, name)
      else
        Shuttle.log "Satellite not found (#{domain})"
      end
    end
    
    def satellites
      Shuttle.system.satellites
    end
    
    def queued_jobs
      queued_jobs = []
      Shuttle.system.queue.each do |job, canceled, immediated|
        job_delay = job.delay
        queued_jobs.push([job.object_id, job.name, canceled, immediated, job.running?, job.waiting?, job_delay])
      end
      queued_jobs
    end
    
    def cancel_job(id)
      Shuttle.system.queue.cancel(id)
    end
    
    def immediate_job(id)
      Shuttle.system.queue.immediate(id)
    end
    
  end
end
