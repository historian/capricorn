require 'rubygems'
require 'rbconfig'
require 'thor'
require 'simple-daemon'
require 'drb'
require 'drb/ssl'

autoload :URI, 'uri'

module Shuttle
  base = File.expand_path(File.dirname(__FILE__))
  
  autoload :App,           (base+'/shuttle/app')
  autoload :Server,        (base+'/shuttle/server')
  autoload :Config,        (base+'/shuttle/config')
  autoload :Client,        (base+'/shuttle/client')
  autoload :System,        (base+'/shuttle/system')
  autoload :Adapter,       (base+'/shuttle/adapter')
  autoload :Satellite,     (base+'/shuttle/satellite')
  autoload :AppRunner,     (base+'/shuttle/app_runner')
  autoload :SystemProfile, (base+'/shuttle/system_profile')
  
  module Adapters
    base = File.expand_path(File.dirname(__FILE__))
    
    autoload :BaseAdapter,      (base+'/shuttle/adapters/base_adapter')
    autoload :MysqlAdapter,     (base+'/shuttle/adapters/mysql_adapter')
    autoload :PleskAdapter,     (base+'/shuttle/adapters/plesk_adapter')
    autoload :ApacheAdapter,    (base+'/shuttle/adapters/apache_adapter')
    autoload :Sqlite3Adapter,   (base+'/shuttle/adapters/sqlite3_adapter')
    autoload :PassengerAdapter, (base+'/shuttle/adapters/passenger_adapter')
  end
  
  DEFAULT_CONFIG_DIR = '/var/shuttle'
  STOP_STATUS    = 101
  RESTART_STATUS = 102
  RELOAD_STATUS  = 103
  QUICK_CERT     = "http://segment7.net/projects/ruby/QuickCert/QuickCert-1.0.2.tar.gz"
  
  def self.client
    unless @client
      @client = Shuttle::Client.shared
      unless @client
        puts "Failed to connect to the shuttle!"
        exit(1)
      end
    end
    @client
  end
  
  def self.config(dir=nil)
    @config = Shuttle::Config.new(dir || Shuttle::DEFAULT_CONFIG_DIR) if dir
    @config
  end
  
  def self.system
    @system ||= Shuttle::System.shared
  end
  
  def self.system_profile
    @active_profile ||= Shuttle::SystemProfile.active_profile
  end
  
  def self.log(*msgs)
    msgs.each do |msg|
      puts msg
    end
    $stdout.flush
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
  
end