class Capr::Git::FindChanges < Capr::Do::Action(:start)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, options={})
    @url, @branch, @since = url, (options[:branch] || 'master'), options[:since]
  end

  def start
    work_tree = work_tree(@url, @branch)
    cmd = exec('git', 'log', '--format', 'format:@---',
                             '--no-color', '--no-renames', '--name-status',
                             ("#{@since}..HEAD" if @since),
                             :pwd => work_tree)

    cmd.callback do
      parse_output(cmd.output)
    end

    cmd.errback do
      puts cmd.output

      fire_message(
        :success => false,
        :message => "Failed to find changed files for #{@url}",
        :verbose => cmd.output)
      fail
    end
  end

  def parse_output(out)
    changes = out.split("\n").inject([]) do |m, l|
      next(m) if l == "@---" or l == ""
      m.unshift l.split(/\s+/, 2)
      m
    end

    files = {}
    changes.each do |(type, path)|
      (files[path] ||= []) << type
    end

    files.each do |path, changes|
      if changes.first == 'A' and changes.last == 'D'
        files.delete(path)
      elsif changes.include?('A')
        files[path] = 'A'
      else
        files[path] = changes.last
      end
    end

    created = []
    updated = []
    deleted = []

    files.each do |path, type|
      case type
      when 'A' then created << path
      when 'M' then updated << path
      when 'D' then deleted << path
      end
    end

    created = created.sort.uniq
    updated = updated.sort.uniq
    deleted = deleted.sort.uniq

    succeed(:created => created,
            :updated => updated,
            :deleted => deleted)
  end

end