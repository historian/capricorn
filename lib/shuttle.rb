require 'rubygems'

autoload :Etc,         'etc'
autoload :DRb,         'drb'
autoload :TSort,       'tsort'
autoload :Logger,      'logger'
autoload :OpenSSL,     'drb/ssl'
autoload :FileUtils,   'fileutils'
autoload :DRbUndumped, 'drb'

module Shuttle
  base = File.expand_path(File.dirname(__FILE__))
  
  autoload :Actor,            (base+'/shuttle/actor')
  autoload :Server,           (base+'/shuttle/server')
  autoload :Client,           (base+'/shuttle/client')
  autoload :System,           (base+'/shuttle/system')
  autoload :JobQueue,         (base+'/shuttle/job_queue')
  autoload :Satellite,        (base+'/shuttle/satellite')
  autoload :AppRunner,        (base+'/shuttle/app_runner')
  autoload :ExceptionHandler, (base+'/shuttle/exception_handler')
  
  module Actors
    base = File.expand_path(File.dirname(__FILE__))
    
    autoload :BaseActor,      (base+'/shuttle/actors/base_actor')
    autoload :MysqlActor,     (base+'/shuttle/actors/mysql_actor')
    autoload :PleskActor,     (base+'/shuttle/actors/plesk_actor')
    autoload :ApacheActor,    (base+'/shuttle/actors/apache_actor')
    autoload :Sqlite3Actor,   (base+'/shuttle/actors/sqlite3_actor')
    autoload :PassengerActor, (base+'/shuttle/actors/passenger_actor')
  end
  
  module Apps
    base = File.expand_path(File.dirname(__FILE__))
    
    autoload :Dev,       (base+'/shuttle/apps/dev')
    autoload :Jobs,      (base+'/shuttle/apps/jobs')
    autoload :Server,    (base+'/shuttle/apps/server')
    autoload :Engines,   (base+'/shuttle/apps/engines')
    autoload :Satellite, (base+'/shuttle/apps/satellite')
  end
  
  DEFAULT_ROOT_SYSTEM_DIR = '/var/shuttle'
  DEFAULT_USER_SYSTEM_DIR = '~/.shuttle'
  STOP_STATUS    = 101
  RESTART_STATUS = 102
  RELOAD_STATUS  = 103
  QUICK_CERT     = "http://segment7.net/projects/ruby/QuickCert/QuickCert-1.0.2.tar.gz"
  
  THOR_VERSION          = '>= 0.9.9'
  RUBIGEN_VERSION       = '>= 1.5.2'
  SIMPLE_DEAMON_VERSION = '>= 0.1.2'
  
  Shuttle::ExceptionHandler.setup
  extend ExceptionHandler
  
  def self.client(token=nil)
    unless @client
      @client = Shuttle::Client.current(token)
      unless @client
        puts "Failed to connect to the shuttle!"
        exit(1)
      end
    end
    @client
  end
  
  def self.system
    @system ||= Shuttle::System.shared
  end
  
  def self.version
    unless @version
      if __FILE__ =~ /\/shuttle-([^\/]+)\//
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
