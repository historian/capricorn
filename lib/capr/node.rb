class Capr::Node

  attr_reader :config

  def self.run(node_root)
    new(node_root).run
  end

  def initialize(node_root)
    @config = Capr::Config.new(node_root)
    Capr.const_set('NODE', self)
  end

  def run
    Rack::Handler::Thin.run self, :Port => config.port
  end

  def call(env)
    dispatcher.call(env)
  rescue Exception => e
    puts "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}"
  end

  def dispatcher
    @routes = Usher::Interface.for(:rack) do
      add('/ping').to(Capr::CGI::Ping)
    end
  end

end