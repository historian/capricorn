class Capricorn::CLI::Applications < Capricorn::CLI

  require 'public_suffix_service'

  include Capricorn::Helpers

  namespace :apps

  desc "init", "initialize the .capricorn.yml file"
  def init
    if File.file?('.capricorn.yml')
      halt ".capricorn.yml already exists"
    end

    File.open(".capricorn.yml", 'w+') do |file|
      file.write <<-EOS
staging:
  cluster:     CLUSTER_NAME
  machine:     MACHINE_ID
  application: APPLICATION_ID
EOS
    end
    info "initialized .capricorn.yml"
  end

  desc "list", "list all applications on MACHINE"
  def list
    apps = client.call.applications.all(machine.to_sym).last || []
    apps.collect! do |app|
      [app[3], app[1]]
    end
    apps.sort! do |a,b|
      a[0] <=> b[0]
    end
    width = apps.inject(0) { |m, (n, _)| (m > n.size ? m : n.size) }

    apps.each do |(name, id)|
      padding = ' ' * (width - name.size)
      puts "#{name}#{padding} : #{id}"
    end
  end

  desc "show", "get info of an application on MACHINE"
  def show
    puts "           Id: #{application_info[1]}"
    puts "      Machine: #{application_info[2]}"
    puts "         Name: #{application_info[3]}"
    puts "  Environment: #{application_info[5]}"
    puts "         User: #{application_info[6]}"
    puts "        Group: #{application_info[7]}"
    puts "         Root: #{application_info[8]}"
  end

  desc "create NAME ENVIRONMENT DOMAINS...", "create an application on MACHINE"
  def create(name, environment, *domains)
    name = name.to_s.strip
    if name.empty?
      halt "Please choose a name!"
    end

    environment = environment.to_s.strip
    unless %w( production development staging testing ).include?(environment)
      halt "Please select a valid environment!"
    end
    environment = environment.to_sym

    domains = domains.flatten do |domain|
      if domain
        domain = domain.to_s.downcase.strip.sub(/^www\./, '')
        begin
          PublicSuffixService.parse(domain).to_s
        rescue Exception
          halt "invalid domain: #{domain}"
        end
      end
    end.compact.uniq
    domains.delete('')
    if domains.empty?
      halt "Please add at least one domain!"
    end

    p client.call.applications.create(machine.to_sym, name, domains, environment)
  end

  desc "import NAME ENVIRONMENT DOMAINS...", "import an application on MACHINE (only use this from an application dir)"
  def import(name, environment, *domains)
    root = File.expand_path(Dir.pwd)

    unless File.file?(File.join(root, "host/config/milkshake.yml"))
      info 'no milkshake config was found'
      halt 'This doesn\'t appear to be a milkshake app!'
    end

    unless File.directory?(File.join(root, "shared"))
      info 'no shared dir was found'
      halt 'This doesn\'t appear to be a milkshake app!'
    end

    gems = YAML.load_file(File.join(root, "host/config/milkshake.yml"))
    gems = (gems['gems'].keys rescue [])
    if gems.empty?
      info 'no gems were found'
      halt 'This doesn\'t appear to be a milkshake app!'
    end

    stats = (File.stat(File.join(root, "host/config/environment.rb")) rescue nil)
    unless stats
      info 'no environment.rb was found'
      halt 'This doesn\'t appear to be a milkshake app!'
    end

    uid = (Etc.getpwuid(stats.uid).name rescue nil)
    unless uid
      info 'no uid was found'
      halt 'This doesn\'t appear to be a milkshake app!'
    end

    gid = (Etc.getgrgid(stats.gid).name rescue nil)
    unless gid
      info 'no gid was found'
      halt 'This doesn\'t appear to be a milkshake app!'
    end

    name = name.to_s.strip
    if name.empty?
      halt "Please choose a name!"
    end

    environment = environment.to_s.strip
    unless %w( production development staging testing ).include?(environment)
      halt "Please select a valid environment!"
    end
    environment = environment.to_sym

    domains = domains.flatten do |domain|
      if domain
        domain = domain.to_s.downcase.strip.sub(/^www\./, '')
        begin
          PublicSuffixService.parse(domain).to_s
        rescue Exception
          halt "invalid domain: #{domain}"
        end
      end
    end.compact.uniq
    domains.delete('')
    if domains.empty?
      halt "Please add at least one domain!"
    end

    p client.call.applications.import(machine.to_sym, name, domains, environment, root, gems, uid, gid)
  end

  desc "fupdate", "force update an application on MACHINE"
  def fupdate
    machine, id = *application
    p client.call.applications.fupdate(machine.to_sym, id)
  end

  desc "relink", "relink an application on MACHINE"
  def relink
    machine, id = *application
    p client.call.applications.relink(machine.to_sym, id)
  end

  desc "restart", "restart an application on MACHINE"
  def restart
    machine, id = *application
    p client.call.applications.restart(machine.to_sym, id)
  end

  desc "start", "start an application on MACHINE"
  def start
    machine, id = *application
    p client.call.applications.start(machine.to_sym, id)
  end

  desc "stop", "stop an application on MACHINE"
  def stop
    machine, id = *application
    p client.call.applications.stop(machine.to_sym, id)
  end

end