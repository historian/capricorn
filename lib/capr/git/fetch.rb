class Capr::Git::Fetch < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, refspec=nil)
    @url = url
    @refspec = refspec # || "+refs/heads/*:refs/heads/*"
  end

  def start
    path = git_dir(url)
    cmd  = exec('git', '--git-dir', path, 'fetch', @url, @refspec)

    cmd.callback do
      fire_message(
        :success => true,
        :message => "Fetched #{@url}",
        :verbose => cmd.output)
      succeed
    end

    cmd.errback do
      fire_message(
        :success => false,
        :message => "Failed to fetch #{@url}",
        :verbose => cmd.output)
      fail
    end
  end

end