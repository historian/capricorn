class Capr::Httpd::UpdateConfig < Capr::Do::Action(:get_repo)

  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def get_repo
    repo_path = config.config_repo_path
    repo_url  = config.config_repo
    action    = nil

    if File.directory?(repo_path)
      action = Capr::Git::Pull.new(repo_path, repo_url, 'master')
    else
      action = Capr::Git::Clone.new(repo_path, repo_url)
    end

    action.on_message &method(:fire_message)
    action.callback   &method(:reset_config)
    action.errback    &method(:fail)
    action.call
  end
  
  def reset_config
    config.reset!
    succeed
  end

end