require File.dirname(__FILE__)+'/thor_extentions'

module Shuttle
  
  # AppRunner allows us to have multiple apps in different namespaces
  class AppRunner < Thor
    
    method_options :config => :optional
    def initialize(opts = {}, *args)
      super
      if opts[:config]
        @root_path = opts[:config]
      else
        args.each_with_index do |arg, idx|
          if arg =~ /^(-c|--config)$/
            @root_path = args[idx+1]
          elsif arg =~ /^(-c|--config)=(.+)$/
            @root_path = $2
          end
        end
      end
      
      Shuttle.config(@root_path || Shuttle::DEFAULT_CONFIG_DIR)
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
        display_klasses(false, classes)
      end
    end
    
    # forward actions to the sub apps
    def method_missing(meth, *args)
      meth = meth.to_s
      super(meth.to_sym, *args) unless meth.include?(?:)
      
      task = Thor[meth]
      task.parse(task.klass.new, ARGV[1..-1])
    end
    
    private
    
    def display_klasses(with_modules = false, klasses = Thor.subclasses)
      klasses -= [Thor, Shuttle::AppRunner] unless with_modules
      raise Error, "No Thor tasks available" if klasses.empty?
      
      if with_modules && !thor_yaml.empty?
        max_name = thor_yaml.max { |(xk, xv), (yk, yv)| xk.to_s.size <=> yk.to_s.size }.first.size
        modules_label    = "Modules"
        namespaces_label = "Namespaces"
        column_width     = [max_name + 4, modules_label.size + 1].max
        
        print "%-#{column_width}s" % modules_label
        puts namespaces_label
        print "%-#{column_width}s" % ("-" * modules_label.size)
        puts "-" * namespaces_label.size
        
        thor_yaml.each do |name, info|
          print "%-#{column_width}s" % name
          puts info[:constants].map { |c| Thor::Util.constant_to_thor_path(c) }.join(", ")
        end
      
        puts
      end
      
      # Calculate the largest base class name
      max_base = klasses.max do |x,y| 
        Thor::Util.constant_to_thor_path(x.name).size <=> Thor::Util.constant_to_thor_path(y.name).size
      end.name.size
      
      # Calculate the size of the largest option description
      max_left_item = klasses.max do |x,y| 
        (x.maxima.usage + x.maxima.opt).to_i <=> (y.maxima.usage + y.maxima.opt).to_i
      end
      
      max_left = max_left_item.maxima.usage + max_left_item.maxima.opt
      
      unless klasses.empty?
        puts # add some spacing
        klasses.each { |k| display_tasks(k, max_base, max_left); }
      else
        puts "\033[1;34mNo Thor tasks available\033[0m"
      end
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
