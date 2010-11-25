class Capr::Git::Pull < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, options={})
    @url, @branch = url, (options[:branch] || 'master')
  end

  def start
    work_tree = work_tree(@url, @branch)
    cmd = exec('git', 'pull', 'origin', @branch,
               :pwd => work_tree)

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