module Capricorn::Helpers

  require 'abbrev'

  DEFAULT_CONFIG = <<-EOH
cluster_name:
  host: localhost
  port: 3457
  username: dummy
  password: dummy
EOH

  def application_ids
    @application_ids ||= begin
      apps = applications.collect do |app|
        app[1]
      end
      apps.flatten!
      apps
    end
  end

  def applications
    @applications ||= begin
      client.call.applications.all(machine.to_sym).last || []
    end
  end

  def machines
    @machines ||= (client.call.machines.all || []).collect do |machine|
      machine.to_s
    end
  end

  def nodes
    @nodes ||= (client.call.runtime.nodes || []).collect do |node|
      node.to_s
    end
  end

  def application_info
    @application_info ||= begin
      apps = applications
      app = apps.detect do |app|
        app[1] == application.last
      end

      unless app
        halt 'Application not found!'
      end

      app
    end
  end

  def application
    @application ||= begin
      application = nil
      if options.application
        application = options.application
      elsif environment['application']
        application = environment['application']
      elsif application_ids.size == 1
        application = application_ids.first
        info "Selected default application: #{application}"
      else
        halt "Please select a application!"
      end

      if application
        application = application_ids.abbrev[application]
      end

      unless application_ids.include?(application)
        halt "I don't know this application!"
      end

      [machine, application]
    end
  end

  def machine
    @machine ||= begin
      machine = nil
      if options.machine
        machine = options.machine
      elsif environment['machine']
        machine = environment['machine']
      elsif machines.size == 1
        machine = machines.first
        info "Selected default machine: #{machine}"
      else
        halt "Please select a machine!"
      end

      if machine
        machine = machines.abbrev[machine]
      end

      unless machines.include?(machine)
        halt "I don't know this machine!"
      end

      machine
    end
  end

  def node
    @node ||= begin
      node = nil
      if options.machine
        node = options.machine
      elsif environment['machine']
        node = environment['machine']
      elsif nodes.size == 1
        node = nodes.first
        info "Selected default node: #{node}"
      else
        halt "Please select a node!"
      end

      if node
        node = nodes.abbrev[node]
      end

      unless nodes.include?(node)
        halt "I don't know this node!"
      end

      node
    end
  end

  def cluster
    @cluster ||= begin
      cluster = nil
      if options.cluster
        cluster = options.cluster
      elsif environment['cluster']
        cluster = environment['cluster']
      elsif config.size == 1
        cluster = config.keys.first
        info "Selected default cluster: #{cluster}"
      else
        halt "Please select a cluster!"
      end

      if cluster
        cluster = config.keys.abbrev[cluster]
      end

      unless config.keys.include?(cluster)
        halt "I don't know this cluster!"
      end

      config[cluster]
    end
  end

  def environment
    @environment ||= begin
      environment = nil
      if options.environment
        environment = options.environment
      elsif local_config.size == 1
        environment = local_config.keys.first
        info "Selected default environment: #{environment}"
      elsif local_config.size > 1
        halt "Please select an environment!"
      end

      if environment
        environment = local_config.keys.abbrev[environment]
      end

      config = nil
      if environment.nil?
        config = {}
      elsif local_config.keys.include?(environment)
        config = local_config[environment]
      else
        halt "I don't know this environment!"
      end
      config
    end
  end

  def local_config
    @local_config ||= begin
      if File.file?('.capricorn.yml')
        YAML.load_file('.capricorn.yml')
      else
        {}
      end
    end
  end

  def config
    @config ||= begin
      config_path = File.expand_path('~/.capricorn/config.yml')
      unless File.file?(config_path)
        FileUtils.mkdir_p(File.dirname(config_path))
        File.open(config_path, 'w+', 0600) { |file| file.write DEFAULT_CONFIG }
        info "Please edit your config file in ~/.capricorn/config.yml"
        exit 2
      end
      YAML.load_file(config_path)
    end
  end

  def client
    host    = (cluster['host'] || 'localhost').to_s
    port    = (cluster['port'] || 3457).to_i
    @client ||= BERTRPC::Service.new(host, port)
  end

  def info(msg)
    shell.say_status('Info', msg)
  end

  def halt(msg, continue=false)
    shell.say_status('Error', msg, :red)
    exit(1) unless continue
  end
end