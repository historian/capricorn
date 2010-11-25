class Capr::Git::Rebase < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, local_branch, remote_branch)
    @url, @local_branch, @remote_branch = url, local_branch, remote_branch
  end

  def start
    git_dir   = git_dir(@url)
    work_tree = work_tree(@url, @local_branch)
    cmd = exec('git', '--git-dir',   git_dir,
                      '--work-tree', work_tree,
                      'rebase', @remote_branch, @local_branch)

    cmd.callback do
      fire_message(cmd.output)
      succeed
    end

    cmd.errback do
      fire_message(cmd.output)
      fail
    end
  end

end