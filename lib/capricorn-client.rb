module Capricorn
  require 'thor'
  require 'yaml'
  require 'bertrpc'
  require 'fileutils'

  require 'capricorn-client/helpers'

  class CLI  < Thor
    namespace "default"

    require 'capricorn-client/cli/applications'
    require 'capricorn-client/cli/applications/domains'
    require 'capricorn-client/cli/applications/gems'
    require 'capricorn-client/cli/gems'
    require 'capricorn-client/cli/machines'

    def self.banner(task)
      "#{banner_base} #{task.formatted_usage(self, true)}"
    end

    def help(meth=nil)
      if meth && !self.respond_to?(meth)
        klass, task = Thor::Util.find_class_and_task_by_namespace(meth)
        klass.start(["-h", task].compact, :shell => self.shell)
      else
        super
      end
    end

    def method_missing(meth, *args)
      meth = meth.to_s
      klass, task = Thor::Util.find_class_and_task_by_namespace(meth)
      args.unshift(task) if task
      klass.start(args, :shell => self.shell)
    end

    class_option :cluster,     :type => :string, :aliases => ['-c']
    class_option :machine,     :type => :string, :aliases => ['-m', '-n', '--node']
    class_option :application, :type => :string, :aliases => ['-a', '--app']
    class_option :environment, :type => :string, :aliases => ['-e', '--env']

    desc "deploy", "deploy an new gem"
    def deploy
      last = Dir.glob("pkg/*.gem").sort do |a, b|
        File.stat(a).mtime <=> File.stat(b).mtime
      end.last

      $capr_gems_weak_push = true

      invoke "gems:push", [last]
      invoke "apps:fupdate"
    end

  end

end