class Capricorn::CLI::Applications < Thor
  include Capricorn::Helpers
  
  desc "list", "list all applications on MACHINE"
  def list
    machine = select_machine
    
    apps = client.call.applications.all(machine.to_sym).last || []
    apps.collect! do |app|
      [app[1], app[3]]
    end
    apps.flatten!
    
    ui.table %w( Id Name ), apps, :columns_across, 2
  end
  
  desc "info", "get info of an application on MACHINE"
  def info
    machine, id = *select_application
    app = find_app(machine, id)
    show_app(app)
  end
  
  desc "create", "create an application on MACHINE"
  def create
    machine     = select_machine
    name        = ui.ask("name: ") do |q|
      q.validate = /^.+$/
    end
    environment = ui.choose do |q|
      q.choices(*%w( production development staging testing ))
      q.prompt = "environment: "
      q.default = 'production'
    end
    domains = []
    domains << ui.ask("domain: ") do |q|
      q.validate  = /^[\w\d]+(\.[\w\d]+)*$/
    end
    loop do
      domain = ui.ask("more:   ") do |q|
        q.validate = /^([\w\d]+(\.[\w\d]+)*)?$/
      end
      break if domain.strip.empty?
      domains << domain
    end
    
    domains = domains.flatten.compact
    domains = domains.collect { |d| d.downcase.sub(/^www\./, '') }
    domains = domains.uniq
    
    environment = environment.to_sym
    
    p client.call.applications.create(machine.to_sym, name, domains, environment)
  end
  
  desc "import", "import an application on MACHINE (only use this from an application dir)"
  def import
    root = File.expand_path(Dir.pwd)
    
    unless File.file?(File.join(root, "host/config/milkshake.yml"))
      puts ui.color('This doesn\'t appear to be a milkshake app!', :red)
      puts ui.color('no milkshake config was found', :red)
      exit 1
    end
    
    unless File.directory?(File.join(root, "shared"))
      puts ui.color('This doesn\'t appear to be a milkshake app!', :red)
      puts ui.color('no shared dir was found', :red)
      exit 1
    end
    
    gems = YAML.load_file(File.join(root, "host/config/milkshake.yml"))
    gems = (gems['gems'].keys rescue [])
    if gems.empty?
      puts ui.color('This doesn\'t appear to be a milkshake app!', :red)
      puts ui.color('no gems were found', :red)
      exit 1
    end
    
    stats = (File.stat(File.join(root, "host/config/environment.rb")) rescue nil)
    unless stats
      puts ui.color('This doesn\'t appear to be a milkshake app!', :red)
      puts ui.color('no environment.rb was found', :red)
      exit 1
    end
    
    uid = (Etc.getpwuid(stats.uid).name rescue nil)
    unless uid
      puts ui.color('This doesn\'t appear to be a milkshake app!', :red)
      puts ui.color('no uid was found', :red)
      exit 1
    end
    
    gid = (Etc.getgrgid(stats.gid).name rescue nil)
    unless gid
      puts ui.color('This doesn\'t appear to be a milkshake app!', :red)
      puts ui.color('no gid was found', :red)
      exit 1
    end
    
    machine     = select_machine
    name        = ui.ask("name: ") do |q|
      q.validate = /^.+$/
    end
    environment = ui.choose do |q|
      q.choices(*%w( production development staging testing ))
      q.prompt = "environment: "
      q.default = 'production'
    end
    domains = []
    domains << ui.ask("domain: ") do |q|
      q.validate  = /^[\w\d]+(\.[\w\d]+)*$/
    end
    loop do
      domain = ui.ask("more:   ") do |q|
        q.validate = /^([\w\d]+(\.[\w\d]+)*)?$/
      end
      break if domain.strip.empty?
      domains << domain
    end
    
    domains = domains.flatten.compact
    domains = domains.collect { |d| d.downcase.sub(/^www\./, '') }
    domains = domains.uniq
    
    environment = environment.to_sym
    
    p client.call.applications.import(machine.to_sym, name, domains, environment, root, gems, uid, gid)
  end
  
  desc "update", "update an application on MACHINE"
  def update
    machine, id = *select_application
    app = find_app(machine, id)
    
    app[4]  ||= []
    app[9]  ||= []
    app[10] ||= []
    
    loop do
      print "\e[H\e[2J" ; $stdout.flush
      
      show_app(app)
      
      puts
      
      ui.choose do |q|
        q.header = "Actions"
        q.prompt = 'What do you wan to do? '
        
        q.choice('gem-add') do
          name = ui.ask("enter a gem name: ") do |q|
            q.validate = /^([a-zA-Z0-9_-]+)?$/
          end
          app[10] << name unless name.strip.empty?
        end
        
        q.choice('gem-remove') do
          g = ui.choose do |q|
            q.header = "Gems"
            q.prompt = 'choose a gem to remove: '
            q.choices(*app[10])
          end
          app[10].delete(g)
        end
        
        q.choice('domain-add') do
          domain = ui.ask("enter a domain: ") do |q|
            q.validate = /^([\w\d]+(\.[\w\d]+)*)?$/
          end
          app[4] << domain unless domain.strip.empty?
        end
        
        q.choice('domain-remove') do
          domain = ui.choose do |q|
            q.header = "Domains"
            q.prompt = 'choose a domain to remove: '
            q.choices(*app[4])
          end
          app[4].delete(domain)
        end
        
        q.choice('abort') do
          exit(0)
        end
        
        q.choice('commit') do
          p client.call.applications.update(machine.to_sym, id, app[4], app[10])
          exit(0)
        end
        
      end
    end
  end
  
  desc "fupdate", "force update an application on MACHINE"
  def fupdate
    machine, id = *select_application
    p client.call.applications.fupdate(machine.to_sym, id)
  end
  
  desc "relink", "relink an application on MACHINE"
  def relink
    machine, id = *select_application
    p client.call.applications.relink(machine.to_sym, id)
  end
  
  desc "restart", "restart an application on MACHINE"
  def restart
    machine, id = *select_application
    p client.call.applications.restart(machine.to_sym, id)
  end
  
  desc "start", "start an application on MACHINE"
  def start
    machine, id = *select_application
    p client.call.applications.start(machine.to_sym, id)
  end
  
  desc "stop", "stop an application on MACHINE"
  def stop
    machine, id = *select_application
    p client.call.applications.stop(machine.to_sym, id)
  end
  
private
  
  def find_app(machine ,id)
    apps = client.call.applications.all(machine.to_sym).last || []
    app = apps.detect do |app|
      app[1] == id
    end
    
    unless app
      puts ui.color('Application not found!', :red)
      exit(1)
    end
    
    app
  end
  
  def show_app(app)
    app = app.to_a.dup
    app.shift
    app.pop
    app.collect! do |v|
      case v
      when Array then v.join(', ')
      else v.to_s
      end
    end
    
    ui.table %w( Id Machine Name Domains Environment User Group Root Installed\ Gems Requested\ Gems  ), app, :columns_down, 2
  end
  
end