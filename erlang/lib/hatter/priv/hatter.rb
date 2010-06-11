#!/usr/bin/env ruby

require 'rubygems'
require 'tmpdir'
require 'fileutils'
require 'stringio'
require 'eventmachine'
require 'bert'
require 'bundler'

module Hatter
  class << self
    attr_reader :root, :user, :uid
    
    def root
      @root ||= begin
        ENV['hatter.container.dir'] or begin
          dir = File.join(Dir.tmpdir, "hatter-#{rand(1<<100)}")
          FileUtils.mkdir_p(dir)
          dir
        end
      end
    end
    
    def user
      @user ||= ENV['hatter.container.user'] || 'nobody'
    end
    
    def env
      @env ||= ENV['hatter.container.env'] || 'production'
    end
    
    def uid
      @uid ||= (Etc.getpwnam(user).uid rescue Etc.getpwnam('nobody').uid)
    end
    
    def socket_path
      @socket_path ||= File.expand_path(ENV['hatter.container.socket']) || 'tmp/sockets/container.sock'
    end
    
    def controle_connection
      @controle_connection
    end
    
    def app
      @app
    end
    
    def pre_jail!
      Bundler::Definition
      Bundler::Dependency
      Bundler::Dsl
      Bundler::Environment
      Bundler::Index
      Bundler::Installer
      Bundler::RemoteSpecification
      Bundler::Resolver
      Bundler::Runtime
      Bundler::Settings
      Bundler::SharedHelpers
      Bundler::SpecSet
      Bundler::Source
      Bundler::Specification
      Bundler::UI
    end
    
    def jail!
      Dir.chdir(root)
      Process::Sys.setuid(uid)
    end
    
    def connect!
      @controle_connection = EM.connect_unix_domain(socket_path, ControleConnection)
    end
    
    def load!
      
      if File.file?('.bundle/environment.rb')
        load '.bundle/environment.rb'
      elsif File.file?('Gemfile')
        Bundler.setup(:default, env.to_sym)
      end
      
      require 'rack'
      require 'rack/handler'
      require 'rack/content_length'
      require 'rack/chunked'
      
      @app, @options = Rack::Builder.parse_file('config.ru')
      @app = Rack::Chunked.new(Rack::ContentLength.new(@app))
    end
    
    def boot!
      pre_jail!
      jail!
      connect!
    end
    
  end
  
  class PacketConnection < EM::Connection
    def initialize(*args)
      super
      @remaining = nil
      @buffer = ""
    end
    
    def receive_data(data)
      unless @remaining
        @remaining = data[0,4].unpack('N').first
        data[0,4] = ""
      end
      
      if @remaining > data.size
        @buffer.concat(data)
        @remaining -= data.size
      elsif @remaining < data.size
        @buffer.concat(data[0,@remaining])
        data[0,@remaining] = ''
        @remaining = nil
        
        begin
          receive_packet(@buffer)
        ensure
          @buffer = ""
          receive_data(data)
        end
      elsif @remaining == data.size
        @buffer.concat(data)
        @remaining = nil
        
        begin
          receive_packet(@buffer)
        ensure
          @buffer = ""
        end
      end
    end
    
    def receive_packet(packet)
      
    end
    
    def send_packet(packet)
      packet = packet.force_encoding('ascii-8bit') if packet.respond_to?(:force_encoding)
      send_data([packet.length].pack('N') + packet)
    end
  end
  
  class BertConnection < PacketConnection
    def receive_packet(packet)
      object = BERT.decode(packet)
      receive_object(object)
    end
    
    def receive_object(object)
      
    end
    
    def send_object(object)
      send_packet(BERT.encode(object))
    end
  end
  
  class ControleConnection < Hatter::BertConnection
    def post_init
      Hatter.load!
      send_object(:booted)
    rescue Exception => e
      puts e.inspect
      send_object(t[:boot_error, e.class.to_s, e.message, e.backtrace])
    end
    
    def receive_object(object)
      case object
      when :stop
        EM.stop_event_loop
      when :accept
        EM.connect_unix_domain(Hatter.socket_path, RackConnection)
      end
    end
    
    def log(msg)
      send_object(t[:log, msg.to_s])
    end
  end
  
  class RackConnection < Hatter::BertConnection
    def initialize(*args)
      super
      
      @mode = :object
    end
    
    def receive_object(env)
      env['rack.version']      = Rack::VERSION
      env['rack.url_scheme'] ||= 'http'
      env['rack.multithread']  = true
      env['rack.errors']       = $stderr
      env['rack.multiprocess'] = false
      env['rack.run_once']     = false
      env['rack.input']        = StringIO.new
      
      case env['REQUEST_METHOD']
      when 'POST', 'PUT'
        @input = env['rack.input']
        @mode = :stream
        operation = lambda { Hatter.app.call(env) }
        callback  = lambda { |result| send_result(*result) }
        EM.defer(operation, callback)
      else
        send_result *Hatter.app.call(env)
      end
    end
    
    def receive_stream(packet)
      @input << packet
    end
    
    def receive_packet(packet)
      case @mode
      when :object then super
      when :stream then receive_stream(packet)
      end
    end
    
    def send_result(status, headers, body)
      send_object(t[:resp, status, headers])
      body.each do |chunk|
        send_packet(chunk)
      end
      close_connection_after_writing
    end
  end
end

EM.run { Hatter.boot! }
