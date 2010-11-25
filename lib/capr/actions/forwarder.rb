class Capr::Forwarder < Cramp::Controller::Action

  include Capr::ConfigHelpers
  include Capr::SharedHelpers
  include Capr::ShellHelpers

  before_start :validate_params
  on_start     :receive_ping
  keep_connection_alive :every => 10

  def validate_params
    repo = params['repository']
    unless repo
      error(522, Yajl::Encoder.encode(
        :success => false,
        :message => "Failed to ping #{node}",
        :verbose => "Invalid request"))
    end

    @url      = repo['url']
    @branches = repo['branches']
    unless @url and Array === @branches and !@branches.empty?
      error(522, Yajl::Encoder.encode(
        :success => false,
        :message => "Failed to ping #{node}",
        :verbose => "Invalid request"))
    end
    
    yield
  end

  def respond_with
    [200, {'Content-Type' => 'application/json'}]
  end

  def error(status, body)
    halt(status, {'Content-Type' => 'application/json'}, body)
  end

  def receive_ping
    if @url == self.config.config_repo
      update = update_repo(@url, ["master"])
      
      update.on_message do |message|
        render Yajl::Encoder.encode(message)
      end
      
      update.callback do
        self.config.reset!
        
        ping = forward_ping
        ping.callback { finish }
        ping.errback  { finish }
      end
      
      update.errback do
        self.config.reset!
        
        finish
      end
    else
      ping = forward_ping
      ping.callback { finish }
      ping.errback  { finish }
    end
  end

  def forward_ping
    forward_nodes = nodes.dup
    deferrable    = EM::DefaultDeferrable.new
    forward_ping_to(forward_nodes, deferrable)
  end

  def forward_ping_to(forward_nodes, deferrable)
    node = forward_nodes.shift

    unless node
      deferrable.succeed
      return deferrable
    end

    url  = "http://#{node}/ping"
    http = EM::HttpRequest.new(url).post(
      :body => {'repository'   => { 'url' => @url, 'branches' => @branches }},
      :head => {"Content-Type" => "application/x-www-form-urlencoded"})

    parser = Yajl::Parser.new
    parser.on_parse_complete = lambda do |message|
      render Yajl::Encoder.encode(message)
    end

    # Use Yajl Stream Decoder
    http.stream do |chunk|
      parser << data
    end

    http.callback do
      forward_ping_to(forward_nodes, deferrable)
    end

    http.errback do
      render Yajl::Encoder.encode(
        :success => false,
        :message => "Failed to ping #{node}",
        :verbose => "Connect error")
      forward_ping_to(forward_nodes, deferrable)
    end

    return deferrable
  end

end