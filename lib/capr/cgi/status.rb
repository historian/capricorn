class Capr::CGI::Status < Cramp::Controller::Action

  on_start :render_status

  def respond_with
    [200, {'Content-Type' => 'application/json'}]
  end

  def error(status, body)
    halt(status, {'Content-Type' => 'application/json'}, body)
  end

  def render_status
    config = Capr::NODE.config.local_applications.dup
    config[:_node] = Capr::NODE.config.local_node
    render Yajl::Encoder.encode(config)
  end

end