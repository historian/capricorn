class Capr::CGI::Ping < Cramp::Controller::Action

  before_start :validate_params
  on_start     :receive_ping
  keep_connection_alive :every => 10

  def validate_params
    @forward = !!request.params['forward']

    repo = request.params['repository']
    unless repo
      error(522, Yajl::Encoder.encode(
        :success => false,
        :message => "Failed to ping #{config.node_name}",
        :verbose => "Invalid request"))
      return
    end

    @url      = repo['url']
    @branches = repo['branches']

    if Hash === @branches
      @branches = @branches.sort { |(a,_), (b,_)| a.to_i <=> b.to_i }
      @branches = @branches.map { |(_, v)| v }
    end

    unless @url and Array === @branches and !@branches.empty?
      error(522, Yajl::Encoder.encode(
        :success => false,
        :message => "Failed to ping #{config.node_name}",
        :verbose => "Invalid request"))
      return
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
    action = Capr::Httpd::Ping.new(@forward, @url, @branches)
    action.on_message do |message|
      render Yajl::Encoder.encode(message)
    end
    action.callback &method(:finish)
    action.errback  &method(:finish)
    action.call
  end

end