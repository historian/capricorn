module Capr::Helpers::Shared

  require 'pathname'

  def git_dir(url)
    if url == self.config.config_repo
      File.join(self.config.node_root, 'config', '.git')
    else
      url = url.gsub(/\.git$/, '')
      url = url.gsub(/[^a-zA-Z0-9]+/, '_')
      File.join(self.config.repo_root, "#{url}.git")
    end
  end

  def work_tree(url, branch)
    if url == self.config.config_repo
      path = File.join(self.config.node_root, 'config')
    else
      url    = url.gsub(/\.git$/, '')
      url    = url.gsub(/[^a-zA-Z0-9]+/, '_')
      branch = branch.gsub(/[^a-zA-Z0-9]+/, '_')
      path = File.join(self.config.checkout_root, "#{url}/#{branch}")
    end
    git_dir = Pathname.new(git_dir(url))
    Pathname.new(path).relative_path_from(git_dir).to_s
  end

end