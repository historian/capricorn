class Capr::Httpd::Forward < Capr::Do::Action(:update_config)

  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(forward, repo, branches)
    @forward, @repo, @branches = forward, repo, branches
  end

  def update_config
    if @repo == config.config_repo
      action = Capr::Httpd::UpdateConfig.new
      action.on_message &method(:fire_message)
      action.errback    &method(:fail)
      action.callback   &method(:find_changed_application_configs)
      action.call
    else
      proxy_ping
    end
  end

  def find_changed_application_configs(old_refs)
    action = Capr::Httpd::DiffConfig.new(:since => old_refs['master'])
    action.on_message &method(:fire_message)
    action.errback    &method(:fail)
    action.callback do |changed_configs|
      @changed_configs = changed_configs.dup
      apply_changed_configs
    end
    action.call
  end

  def apply_changed_configs
    if @changed_configs.empty?
      proxy_ping
    else
      changed_config = @changed_configs.shift
      action = Capr::Httpd::ApplyConfig.new(changed_config)
      action.on_message &method(:fire_message)
      action.errback    &method(:apply_changed_configs)
      action.callback   &method(:apply_changed_configs)
      action.call
    end
  end

  def proxy_ping
    if @forward
      nodes  = config.node_hostnames.dup
      action = Capr::Httpd::ProxyPing.new(nodes, @repo, @branches)
      action.on_message &method(:fire_message)
      action.callback   &method(:succeed)
      action.errback    &method(:fail)
      action.call
    else
      succeed
    end
  end

end