module Capricorn
  gem 'fd-bertrpc'

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
    require 'capricorn-client/cli/builder'
    require 'capricorn-client/cli/deployer'
    require 'capricorn-client/cli/releaser'

    def self.start(given_args = ARGV, config = {})
      if self == Capricorn::CLI
        case given_args[0]
        when 'deploy'
          given_args[0] = 'deploy:version'
        when 'build'
          given_args[0] = 'build:current'
        end
      end
      super(given_args, config)
    end

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
      if self.class == Capricorn::CLI
        meth = meth.to_s
        klass, task = Thor::Util.find_class_and_task_by_namespace(meth)
        args.unshift(task) if task
        klass.start(args, :shell => self.shell)
      else
        super
      end
    end

    class_option :cluster,     :type => :string, :aliases => ['-c']
    class_option :machine,     :type => :string, :aliases => ['-m', '-n', '--node']
    class_option :application, :type => :string, :aliases => ['-a', '--app']
    class_option :environment, :type => :string, :aliases => ['-e', '--env']

  end

end