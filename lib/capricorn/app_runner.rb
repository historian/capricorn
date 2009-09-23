Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

require File.dirname(__FILE__)+'/extentions/thor_extentions'

module Capricorn
  
  # AppRunner allows us to have multiple apps in different namespaces
  class AppRunner < Thor
    
    def self.subapps
      @subapps ||= []
    end
    
    # register a subapplication with the app runner.
    def self.use(app)
      self.subapps.push Capricorn::Apps.const_get(app)
    end
    
    self.tasks['help'] = Thor.tasks['help']
    Thor.remove_task "help"
    
    # Override Thor#help so we can give info about not-yet-loaded tasks
    def help(task=nil)
      if task
        klass, task = Thor::Util.namespace_to_thor_class_and_task(task, false)
        klass.help(shell, task, :namespace => true)
      else
        self.class.subapps.each do |klass|
          shell.say("== #{Thor::Util.namespace_from_thor_class(klass).split(':').last}", Thor::Shell::Color::BLUE)
          klass.help(shell, task, :namespace => true)
        end
        shell.say("== general", Thor::Shell::Color::BLUE)
        self.class.help(shell, task, :namespace => task && task.include?(?:))
      end
    end
    
    # forward actions to the sub apps
    def method_missing(meth, *args)
      meth = meth.to_s
      unless meth.include?(?:)
        super(meth.to_sym, *args)
      else
        
        klass, task = Thor::Util.namespace_to_thor_class_and_task(meth)
        argv = ORIGINAL_ARGV.dup
        argv[0] = task
        begin
          klass.start(argv)
        rescue
          puts $!.message
        end
        
      end
    end
    
  end
end
