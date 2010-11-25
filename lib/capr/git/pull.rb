class Capr::Git::Pull < Capr::Do::Action(:start)

  include Capr::Helpers::Shell

  define_callback :message

  def initialize(path, url, branch)
    @path, @url, @branch = path, url, branch
  end

  def start
    cmd = exec('git', '--git-dir', File.join(@path, '.git'),
                      '--work-tree', '..',
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