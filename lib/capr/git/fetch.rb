class Capr::Git::Fetch < Capr::Do::Action(:start)

  include Capr::Helpers::Shell

  define_callback :message

  def initialize(path, url, refspec=nil)
    @path, @url = path, url
    @refspec = refspec # || "+refs/heads/*:refs/heads/*"
  end

  def start
    cmd = exec('git', '--git-dir', @path, 'fetch', @url, @refspec)

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