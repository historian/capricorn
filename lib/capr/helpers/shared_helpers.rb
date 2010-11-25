module Capr::SharedHelpers

  def repo_path_for_url(url)
    File.expand_path(dirname_for_url(url), self.config.repo_root)
  end

  def branch_path_for_and_branch(url, branch)
    File.expand_path(dirname_for_url_and_branch(url, branch), self.config.checkout_root)
  end

  def dirname_for_url(url)
    url = url.gsub(/\.git$/, '')
    url = url.gsub(/[^a-zA-Z0-9]+/, '_')
    "#{url}.git"
  end

  def dirname_for_url_and_branch(url, branch)
    url    = url.gsub(/\.git$/, '')
    url    = url.gsub(/[^a-zA-Z0-9]+/, '_')
    branch = branch.gsub(/[^a-zA-Z0-9]+/, '_')
    "#{url}/#{branch}"
  end

end