class Capr::Httpd::UpdateConfig < Capr::Do::Action(:record_refs)

  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def record_refs
    repo_path = config.config_repo_path
    repo_url  = config.config_repo

    if File.directory?(repo_path)
      action = Capr::Git::RecordRefs.new(repo_url, :branch => 'master')
      action.on_message &method(:fire_message)
      action.callback   do |refs|
        @refs = refs
        get_repo
      end
      action.errback &method(:fail)
      action.call
    else
      get_repo
    end
  end

  def get_repo
    repo_path = config.config_repo_path
    repo_url  = config.config_repo
    action    = nil

    if File.directory?(repo_path)
      action = Capr::Git::Pull.new(repo_url, :branch => 'master')
    else
      action = Capr::Git::Clone.new(repo_url, :branch => 'master')
    end

    action.on_message &method(:fire_message)
    action.callback   &method(:reset_config)
    action.errback    &method(:revert)
    action.call
  end

  def reset_config
    config.reset!
    succeed(@refs)
  end

  def revert
    if @refs
      repo_url  = config.config_repo
      action = Capr::Git::ResetRefs.new(repo_url, @refs, :branch => 'master')
      action.on_message &method(:fire_message)
      action.callback   &method(:fail)
      action.errback    &method(:fail)
      action.call
    else
      fail
    end
  end

end