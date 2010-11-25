class Capr::Git::RecordRefs < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, options={})
    @url, @branch = url, options[:branch]
  end

  def start
    cmd = exec('git', 'branch', '--verbose',
                                '--no-color',
                                '--no-abbrev',
                                '-a',
                                :pwd => work_tree(@url, @branch))

    cmd.callback do
      refs = {}
      cmd.output.split("\n").each do |line|
        line = line.sub(/^\*?\s+/, '')
        line = line.split(/\s+/, 3)
        if line.size >= 2
          name = line[0]
          sha1 = line[1]
          refs[name] = sha1 unless name == 'remotes/origin/HEAD'
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

