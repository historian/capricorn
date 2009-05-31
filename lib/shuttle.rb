require 'rubygems'

require File.dirname(__FILE__)+'/shuttle/rubygems_plugin'

module Shuttle
  base = File.expand_path(File.dirname(__FILE__))
  
  autoload :App,           (base+'/shuttle/app')
  autoload :Actor,         (base+'/shuttle/actor')
  autoload :Server,        (base+'/shuttle/server')
  autoload :Client,        (base+'/shuttle/client')
  autoload :System,        (base+'/shuttle/system')
  autoload :Satellite,     (base+'/shuttle/satellite')
  autoload :AppRunner,     (base+'/shuttle/app_runner')
  
  module Actors
    base = File.expand_path(File.dirname(__FILE__))
    
    autoload :BaseActor,      (base+'/shuttle/actors/base_actor')
    autoload :MysqlActor,     (base+'/shuttle/actors/mysql_actor')
    autoload :PleskActor,     (base+'/shuttle/actors/plesk_actor')
    autoload :ApacheActor,    (base+'/shuttle/actors/apache_actor')
    autoload :Sqlite3Actor,   (base+'/shuttle/actors/sqlite3_actor')
    autoload :PassengerActor, (base+'/shuttle/actors/passenger_actor')
  end
  
  DEFAULT_ROOT_SYSTEM_DIR = '/var/shuttle'
  DEFAULT_USER_SYSTEM_DIR = '~/.shuttle'
  STOP_STATUS    = 101
  RESTART_STATUS = 102
  RELOAD_STATUS  = 103
  QUICK_CERT     = "http://segment7.net/projects/ruby/QuickCert/QuickCert-1.0.2.tar.gz"
  
  def self.client
    unless @client
      @client = Shuttle::Client.current
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
  
  def self.server?(value=nil)
    @is_server = value unless value.nil?
    @is_server
  end
  
end
