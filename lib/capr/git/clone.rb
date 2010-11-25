class Capr::Git::Clone < Capr::Do::Action(:start)

  include Capr::Helpers::Shell

  define_callback :message

  def initialize(path, url, bare=false)
    @path, @url, @bare = path, url, bare
  end

  def start
    cmd = exec('git', 'clone', ('--bare' if @bare), url, repo_path)

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