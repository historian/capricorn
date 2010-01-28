require File.dirname(File.expand_path(__FILE__))+'/../../minigems'
require 'bert'

unless Object.respond_to?(:instance_exec)
  class Object
    module InstanceExecHelper; end
    include InstanceExecHelper
    def instance_exec(*args, &block) # !> method redefined; discarding old instance_exec
      mname = "__instance_exec_#{Thread.current.object_id.abs}_#{object_id.abs}"
      InstanceExecHelper.module_eval{ define_method(mname, &block) }
      begin
        ret = __send__(mname, *args)
      ensure
        InstanceExecHelper.module_eval{ undef_method(mname) } rescue nil
      end
      ret
    end
  end
end

module Helpers
  def send(obj)
    berp = BERT.encode(obj)
    $stdout.write [berp.size].pack('N')+berp
    $stdout.flush
  end
  
  def receive
    exit(0) if $stdin.eof?
    h = $stdin.read(4)
    while h == nil
      sleep(0.1)
      exit(0) if $stdin.eof?
      h = $stdin.read(4)
    end
    l = h.unpack('N')[0]
    exit(0) if $stdin.eof?
    BERT.decode($stdin.read(l))
  end
  
  def error(*args)
    raise InternalError, args
  end
  
  extend self
end

class InternalError < RuntimeError
  attr_accessor :args
  def initialize(args)
    @args = args
    super
  end
end

def Erlang(&block)
  $stdin.sync = true
  
  loop do
    cmd = Helpers.receive
    
    if cmd == :stop
      exit(0)
    end
    
    begin
      ctx = Object.new
      ctx.extend Helpers
      ctx.instance_exec(cmd, &block)
    rescue InternalError => e
      Helpers.send BERT::Tuple[:error, BERT::Tuple[*e.args]]
    rescue RuntimeError => e
      Helpers.send BERT::Tuple[:error, BERT::Tuple[:runtime, e.class.to_s, e.message, e.backtrace]]
    rescue Exception => e
      Helpers.send BERT::Tuple[:error, BERT::Tuple[:system, e.class.to_s, e.message, e.backtrace]]
    end
  end
end
