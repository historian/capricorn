module Capricorn
  require 'thor'
  require 'yaml'

  class Util  < Thor
    namespace "default"

    require 'capricorn-util/internal'

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

  end
end