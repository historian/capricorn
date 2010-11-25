class Capr::Git::Pull < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, branch)
    @url, @branch = url, branch
  end

  def start
    git_dir   = git_dir(@url)
    work_tree = work_tree(@url, @branch)
    cmd = exec('git', '--git-dir',   git_dir,
                      '--work-tree', work_tree,
                      'pull', @url, @branch)

    cmd.callback do
      fire_message(
        :success => true,
        :message => "Pulled #{@url}",
        :verbose => cmd.output)
      succeed
    end

    cmd.errback do
      puts cmd.output
      
      fire_message(
        :success => false,
        :message => "Failed to pull #{@url}",
        :verbose => cmd.output)
      fail
    end
  end

end