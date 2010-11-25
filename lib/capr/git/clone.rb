class Capr::Git::Clone < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, options={})
    @url, @bare, @branch = url, (options[:bare] || false), (options[:branch] || 'master')
  end

  def start
    if @bare
      path = git_dir(@url)
    else
      path = work_tree(@url, @branch)
    end
    
    cmd  = exec('git', 'clone', ('--bare' if @bare), url, path)

    cmd.callback do
      fire_message(
        :success => true,
        :message => "Cloned #{@url}",
        :verbose => cmd.output)
      succeed
    end

    cmd.errback do
      fire_message(
        :success => false,
        :message => "Failed to clone #{@url}",
        :verbose => cmd.output)
      fail
    end
  end

end