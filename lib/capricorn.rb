require 'rubygems'

autoload :URI,         'uri'
autoload :Etc,         'etc'
autoload :DRb,         'drb'
autoload :TSort,       'tsort'
autoload :Logger,      'logger'
autoload :OpenSSL,     'drb/ssl'
autoload :FileUtils,   'fileutils'
autoload :DRbUndumped, 'drb'

module Capricorn
  base = File.expand_path(File.dirname(__FILE__))
  
  autoload :Actor,            (base+'/capricorn/actor')
  autoload :Server,           (base+'/capricorn/server')
  autoload :Client,           (base+'/capricorn/client')
  autoload :System,           (base+'/capricorn/system')
  autoload :Daemon,           (base+'/capricorn/daemon')
  autoload :JobQueue,         (base+'/capricorn/job_queue')
  autoload :Satellite,        (base+'/capricorn/satellite')
  autoload :AppRunner,        (base+'/capricorn/app_runner')
  autoload :ExceptionHandler, (base+'/capricorn/exception_handler')
  
  module Actors
    base = File.expand_path(File.dirname(__FILE__))
    
    autoload :BaseActor,      (base+'/capricorn/actors/base_actor')
    autoload :MysqlActor,     (base+'/capricorn/actors/mysql_actor')
    autoload :PleskActor,     (base+'/capricorn/actors/plesk_actor')
    autoload :ApacheActor,    (base+'/capricorn/actors/apache_actor')
    autoload :Sqlite3Actor,   (base+'/capricorn/actors/sqlite3_actor')
    autoload :HostFileActor,  (base+'/capricorn/actors/host_file_actor')
    autoload :PassengerActor, (base+'/capricorn/actors/passenger_actor')
  end
  
  module Apps
    base = File.expand_path(File.dirname(__FILE__))
    
    autoload :Dev,       (base+'/capricorn/apps/dev')
    autoload :Jobs,      (base+'/capricorn/apps/jobs')
    autoload :Server,    (base+'/capricorn/apps/server')
    autoload :Engines,   (base+'/capricorn/apps/engines')
    autoload :Satellite, (base+'/capricorn/apps/satellite')
  end
  
  DEFAULT_ROOT_SYSTEM_DIR = '/var/capricorn'
  DEFAULT_USER_SYSTEM_DIR = '~/.capricorn'
  STOP_STATUS    = 101
  RESTART_STATUS = 102
  RELOAD_STATUS  = 103
  QUICK_CERT     = "http://segment7.net/projects/ruby/QuickCert/QuickCert-1.0.2.tar.gz"
  
  THOR_VERSION          = '= 0.9.9'
  RUBIGEN_VERSION       = '>= 1.5.2'
  
  Capricorn::ExceptionHandler.setup
  extend ExceptionHandler
  
  def self.client(token=nil)
    unless @client
      @client = Capricorn::Client.current(token)
      unless @client
        puts "Failed to connect to the capricorn!"
        exit(1)
      end
    end
    @client
  end
  
  def self.system
    @system ||= Capricorn::System.shared
  end
  
  def self.version
    unless @version
      if __FILE__ =~ /\/capricorn-([^\/]+)\//
        @version = $1
      else
        @version = 'edge'
      end
    end
    @version
  end
  
  def self.server?(value=nil)
    @is_server = value unless value.nil?
    @is_server
  end
  
  def self.runtime_gem(gem, version='>= 0.0.0', lib=nil)
    begin
      gem(gem, version)
      require(lib || gem)
    rescue LoadError
      puts "You must install #{gem} (#{version}) to use this.\nPlease run: [sudo] gem install #{gem}"
      exit(1)
    end
  end
  
end
