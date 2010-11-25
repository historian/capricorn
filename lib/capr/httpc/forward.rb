class Capr::Httpc::Forward < Capr::Do::Action(:start)

  define_callback :message

  def initialize(hostname, repo, branches)
    @hostname, @repo, @branches = hostname, repo, branches

    @parser = Yajl::Parser.new
    @parser.on_parse_complete = lambda do |message|
      fire_message message
    end
  end

  def start
    url  = "http://#{@hostname}/forward"
    
    http = EM::HttpRequest.new(url).post(
      :head => {"Content-Type" => "application/x-www-form-urlencoded"},
      :body => {'repository' => { 'url' => @repo, 'branches' => @branches }})

    http.stream do |chunk|
      begin
        @parser << chunk
      rescue Yajl::ParseError => e
        fire_message(
          :success => false,
          :message => "Invalid response received from #{@hostname}",
          :verbose => "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}")
        fail
      end
    end

    http.callback { succeed }
    http.errback  { fail    }
  end

end