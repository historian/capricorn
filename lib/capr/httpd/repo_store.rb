class Capr::Httpd::RepoStore

  def update(repo_url, refs, channel)
    update_repo
  end

private

  def update_repo(repo_url, channel)
    repo_path = config.bare_repo_path(repo_url)
    if File.directory?(repo_path)
      channel.exec :fetch_repo, repo_url, repo_path
    else
      channel.exec :clone_repo, repo_url, repo_path
    end
  end

  def clone_repo(repo_url, repo_path, channel)
    FileUtil.mkdir_p(File.dirname(repo_path))

    cmd = ['git', 'clone', '--bare', repo_url, repo_path]

    EM.system(Shellwords.join(cmd)) do |output, status|
      if status == 0
        channel.log(:info, "cloned #{repo_url}", output)
        channel.succeed
      else
        FileUtil.rm_rf(repo_path)
        channel.log(:error, "Failed to clone #{repo_url}", output)
        channel.fail
      end
    end
  end

  def fetch_repo(repo_url, repo_path, channel)
    cmd = ['git', '--git-dir', repo_path, 'fetch', '--force']

    EM.system(Shellwords.join(cmd)) do |output, status|
      if status == 0
        channel.log(:info, "fetched #{repo_url}", output)
        channel.succeed
      else
        channel.log(:error, "Failed to fetch #{repo_url}", output)
        channel.fail
      end
    end
  end

  def update_refs(repo_url, refs, channel)
    channel.exec
    refs = refs.dup
    ref  = refs.shift
    if ref
      update = channel.exec :update_ref, repo_url, ref
      update.callback do
        channel.exec :update_refs, repo_url, refs
      end
    else
      channel.succeed
    end
  end

  def update_ref(repo_url, ref, channel)
    ref_path  = config.ref_path(repo_url, ref)
    repo_path = config.bare_repo_path(repo_url)
    if File.directory?(ref_path)
      channel.exec :pull_ref, repo_path, ref, ref_path
    else
      channel.exec :clone_ref, repo_path, ref, ref_path
    end
  end

  def clone_ref(repo_path, ref, ref_path, channel)
    FileUtil.mkdir_p(File.dirname(ref_path))

    cmd = ['git', 'clone', '--shared', '--branch', ref, repo_path, ref_path]

    EM.system(Shellwords.join(cmd)) do |output, status|
      if status == 0
        channel.log(:info, "cloned #{repo_url} -> #{ref}", output)
        channel.succeed
      else
        FileUtil.rm_rf(ref_path)
        channel.log(:error, "Failed to clone #{repo_url} -> #{ref}", output)
        channel.fail
      end
    end
  end

  def pull_ref(repo_path, ref, ref_path, channel)
    cmd = ['git', '--git-dir', ref_path+'/.git', '--work-tree', ref_path, 'pull', 'origin', ref]

    EM.system(Shellwords.join(cmd)) do |output, status|
      if status == 0
        channel.log(:info, "pulled #{repo_url} -> #{ref}", output)
        channel.succeed
      else
        FileUtil.rm_rf(ref_path)
        channel.log(:error, "Failed to pull #{repo_url} -> #{ref}", output)
        channel.fail
      end
    end
  end

  def config
    @config ||= Capr::NODE.config
  end

end