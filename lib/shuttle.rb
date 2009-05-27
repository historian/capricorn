require 'rubygems'
require 'thor'
require 'simple-daemon'
require 'drb'

module Shuttle
  base = File.expand_path(File.dirname(__FILE__))
  
  autoload :App,         (base+'/shuttle/app')
  autoload :Server,      (base+'/shuttle/server')
  autoload :Config,      (base+'/shuttle/config')
  autoload :Client,      (base+'/shuttle/client')
  autoload :System,      (base+'/shuttle/system')
  autoload :Adapter,     (base+'/shuttle/adapter')
  autoload :Satellite,   (base+'/shuttle/satellite')
  
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
  
  def self.app(app=nil)
    @app = app if app
    @app
  end
  
  def self.config(dir=nil)
    @config = Shuttle::Config.new(dir || Shuttle::DEFAULT_CONFIG_DIR) if dir
    @config
  end
  
  def self.system
    @system ||= Shuttle::System.shared
  end
end