class Capr::Git::Rebase < Capr::Do::Action(:start)

  include Capr::Helpers::Shell

  define_callback :message

  def initialize(path, local_branch, remote_branch)
    @path, @local_branch, @remote_branch = path, local_branch, remote_branch
  end

  def start
    cmd = exec('git', '--git-dir', @path, '--work-tree', File.dirname(@path),
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