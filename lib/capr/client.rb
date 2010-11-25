module Capr::Client

  def self.ping(node, repository, branches)
    reactor_was_running = EM.reactor_running?
    EM.run do

      unless node
        raise ArgumentError
      end

      url  = "http://#{node}/forward"
      http = EM::HttpRequest.new(url).post(
        :body => {'repository'   => { 'url' => repository, 'branches' => branches }},
        :head => {"Content-Type" => "application/x-www-form-urlencoded"})

      parser = Yajl::Parser.new
      parser.on_parse_complete = lambda do |message|
        p message
      end

      # Use Yajl Stream Decoder
      http.stream do |chunk|
        parser << chunk
      end

      http.callback do
        EM.stop_event_loop unless reactor_was_running
      end

      http.errback do
        EM.stop_event_loop unless reactor_was_running
      end

    end
    return true
  end

end