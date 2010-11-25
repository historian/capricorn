class Capr::Httpd::Forward < Capr::Do::Action(:update_config)

  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(repo, branches)
    @repo, @branches = repo, branches
  end

  def update_config
    if @repo == config.config_repo
      action = Capr::Httpd::UpdateConfig.new
      action.on_message &method(:fire_message)
      action.callback   &method(:proxy_ping)
      action.errback    &method(:fail)
      action.call
    else
      proxy_ping
    end
  end

  def proxy_ping
    nodes  = config.node_hostnames.dup
    action = Capr::Httpd::ProxyPing.new(nodes, @repo, @branches)
    action.on_message &method(:fire_message)
    action.callback   &method(:succeed)
    action.errback    &method(:fail)
    action.call
  end

end