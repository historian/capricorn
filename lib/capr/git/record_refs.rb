class Capr::Git::Pull < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url)
    @url = url
  end

  def start
    git_dir = git_dir(@url)
    cmd = exec('git', '--git-dir', git_dir,
                      'branch', '--no-color', '--no-abbrev')

    cmd.callback do
      refs = {}
      cmd.output.split("\n").each do |line|
        line = line.sub(/^\*?\s+/, '')
        line = line.split(/\s+/, 3)
        if line.size >= 2
          name = line[0]
          sha1 = line[1]
          refs[name] = sha1
        end
      end
      
      succeed(refs)
    end

    cmd.errback do
      fire_message(
        :success => false,
        :message => "Failed to inspect #{@repo}",
        :verbose => cmd.output)
      fail
    end
  end

end

