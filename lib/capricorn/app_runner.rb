Capricorn.runtime_gem('thor', Capricorn::THOR_VERSION)

require File.dirname(__FILE__)+'/extentions/thor_extentions'

module Capricorn
  
  # AppRunner allows us to have multiple apps in different namespaces
  class AppRunner < Thor
    
    # register a subapplication with the app runner.
    def self.use(app)
      Capricorn::Apps.const_get(app)
    end
    
    # Override Thor#help so we can give info about not-yet-loaded tasks
    def help(task = nil)
      super
      
      unless task
        search = ".*#{search}" if options["substring"]
        search = /^#{search}.*/i
        group  = options[:group] || "standard"
        
        classes = Thor.subclasses.select do |k|
          (options[:all] || k.group_name == group) && 
          Thor::Util.constant_to_thor_path(k.name) =~ search
        end
        display_klasses(classes)
      end
    end
    
    # forward actions to the sub apps
    def method_missing(meth, *args)
      meth = meth.to_s
      super(meth.to_sym, *args) unless meth.include?(?:)
      
      begin
        task = Thor[meth]
        task.parse(task.klass.new, ARGV[1..-1])
      rescue => e
        puts e.message
      end
    end
    
    private
    
    def display_klasses(klasses = Thor.subclasses)
      klasses -= [Thor, Capricorn::AppRunner]
      
      if klasses.empty?
        puts "\033[1;34mNo Thor tasks available\033[0m"
      else
        maxima = column_maxima_for(klasses)
        
        puts # add some spacing
        klasses.each { |k| display_tasks(k, maxima.first, maxima.last) }
      end
    end  
    
    def column_maxima_for(klasses)
      # Calculate the largest base class name
      klass_with_longest_name = klasses.max do |x,y| 
        Thor::Util.constant_to_thor_path(x.name).size <=> Thor::Util.constant_to_thor_path(y.name).size
      end
      max_base = klass_with_longest_name.name.size
      
      # Calculate the size of the largest option description
      max_left_item = klasses.max do |x,y| 
        total_x = (x.maxima.usage + x.maxima.opt).to_i
        total_y = (y.maxima.usage + y.maxima.opt).to_i
        total_x <=> total_y
      end
      
      max_left = max_left_item.maxima.usage + max_left_item.maxima.opt
      
      return [max_base, max_left]
    end
    
    def display_tasks(klass, max_base, max_left)
      if klass.tasks.values.length > 1
        
        base = Thor::Util.constant_to_thor_path(klass.name)
        
        if base.to_a.empty?
          base = 'default' 
          puts "\033[1;35m#{base}\033[0m"
        else
          puts "\033[1;34m#{base}\033[0m"
        end
    
        puts "-" * base.length
        
        klass.tasks.each true do |name, task|
          format_string = "%-#{max_left + max_base + 5}s"
          print format_string % task.formatted_usage(true)
          puts task.description
        end
        
        unless klass.opts.empty?
          puts "\nglobal options: #{Options.new(klass.opts)}"
        end
        
        puts # add some spacing
      end
    end
    
  end
end
