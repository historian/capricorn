class Capr::Node
  
  attr_reader :config
  
  def self.run(node_name, node_root, config_repo, port)
    new(node_name, node_root, config_repo, port).run
  end
  
  def initialize(node_name, node_root, config_repo, port)
    @config = Capr::Config.new(node_name, node_root, config_repo, port)
    Capr.const_set('NODE', self)
  end
  
  def run
    Rack::Handler::Thin.run dispatcher, :Port => config.port
  end
  
  def dispatcher
    @routes = Usher::Interface.for(:rack) do
      add('/forward').to(Capr::Forwarder)
      # add('/ping').to(Capr::Ping)
    end
  end
  
end