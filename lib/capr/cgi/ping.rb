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

  def render(msg)
    super Yajl::Encoder.encode(msg)
  end

  def log(level, message)
    render { "type" => "log", 'type' => level.to_s, 'message' => message }
  end

  def receive_ping
    stack = Stack.new do
      finish
    end

    stack.push method(:update_repository)

    if @url == config.config_repo
      stack.push(method(:update_config))
    else
      stack.push(method(:update_applications))
    end

    stack.run
  end

  def update_repository
    @repo_path = config.repo_path(@url)
    if File.directory?(@repo_path)
      cmd = Shellwords.join([
        'git', '--git-dir', @repo_path + '/.git', '--work-tree', @repo_path,
        'pull', @url, @repo_path])
      EM.system(cmd) do |output, status|
    else
      cmd = Shellwords.join(['git', 'clone', @url, @repo_path])
      EM.system(cmd) do |output, status|
        log(:info, output)
        if status != 0
          FileUtil.rm_rf(@repo_path)
          yield(false)
        else
          yield(true)
        end
      end
    end
  end

  class Stack

    def initialize(&callback)
      @callback = callback
      @actions = []
    end

    def push(action)
      @actions << action
    end

    def run
      action = @actions.shift
      if action
        action.call do |succes|
          if success
            EM.next_tick { self.run }
          else
            @actions = []
            EM.next_tick { self.run }
          end
        }
      else
        @callback.call
      end
    end

  end

end