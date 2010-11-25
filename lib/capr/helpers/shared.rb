module Capr::Helpers::Shared

  def git_dir(url)
    if url == self.config.config_repo
      raise ArgumentError
    else
      url = url.gsub(/\.git$/, '')
      url = url.gsub(/[^a-zA-Z0-9]+/, '_')
      File.join(self.config.repo_root, "#{url}.git")
    end
  end

  def work_tree(url, branch)
    if url == self.config.config_repo
      File.join(self.config.node_root, 'config')
    else
      url    = url.gsub(/\.git$/, '')
      url    = url.gsub(/[^a-zA-Z0-9]+/, '_')
      branch = branch.gsub(/[^a-zA-Z0-9]+/, '_')
      File.join(self.config.checkout_root, "#{url}/#{branch}")
    end
  end

end