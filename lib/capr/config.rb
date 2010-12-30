class Capr::Config

  attr_reader :node_root

  def initialize(node_root)
    @node_root = File.expand_path(node_root)
  end

  def reset!
    (instance_variables - ['@node_root']).each do |ivar|
      instance_variable_set(ivar, nil)
    end

    self
  end

  # =======================
  # = Bootstrapper config =
  # =======================
  def bootstrapper_config
    @bootstrapper_config ||= begin
      path = File.join(node_root, '.config')
      if File.file?(path)
        parts = File.read(path).split("\n")
      else
        parts = []
      end
      parts = parts.map { |part| part.strip }
      {
        'name' => parts[0],
        'repo' => parts[1]
      }
    end
  end

  def config_repo
    bootstrapper_config['repo']
  end

  def node_name
    bootstrapper_config['name']
  end

  # =================
  # = Normal Config =
  # =================
  def path_for_repo(repo)
    File.expand_path(repo.gsub(/[.:_\/-]+/, '_'), node_root)
  end

  def config_file
    @config_file ||= Hash.new do |h, f|
      path = File.join(path_for_repo(config_repo), f)
      h[f] = YAML.load_file(f)
    end
  end

  def node_hostname
    config_file[File.join(node_name, '_node.yml')]['hostname']
  end

  def node_host
    node_hostname.split(':', 2)[0]
  end

  def node_port
    (node_hostname.split(':', 2)[1] || 8181).to_i
  end

end