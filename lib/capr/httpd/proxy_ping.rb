class Capr::Httpd::ProxyPing < Capr::Do::Action(:ping_node)

  define_callback :message

  def initialize(nodes, repo, branches)
    @nodes, @repo, @branches = nodes.dup, repo, branches
  end

  def ping_node
    if @nodes.empty?
      succeed
    else
      node   = @nodes.shift
      action = Capr::Httpc::Ping.new(node, @repo, @branches)
      action.on_message &method(:fire_message)
      action.callback &method(:ping_node)
      action.errback do
        fire_message(
          :success => false,
          :message => "Failed to ping #{node}",
          :verbose => "Connect error")

        ping_node
      end
      action.call
    end
  end

end