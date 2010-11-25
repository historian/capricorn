class Capr::Config

  include Capr::Helpers::Shared

  attr_reader :node_root

  def initialize(node_root)
    @node_root = File.expand_path(node_root)
  end

  def local_node_bootstrapper_config
    @local_node_bootstrapper_config ||= begin
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
    local_node_bootstrapper_config['repo']
  end

  def config_repo_path
    @config_repo_path ||= begin
      File.join(self.node_root, 'config')
    end
  end

  def node_name
    local_node_bootstrapper_config['name']
  end

  def local_node
    @local_node ||= begin
      self.nodes[self.node_name]
    end
  end

  def port
    @port ||= begin
      parts = (self.local_node['hostname'] || '').split(':', 2)
      (parts[1] || 8181).to_i
    end
  end

  def nodes
    @nodes ||= begin
      repo  = config_repo_path
      paths = Dir.glob(File.expand_path('*/_node.yml', repo))

      paths.inject({}) do |m, path|
        name = File.basename(File.dirname(path))
        m[name] = YAML.load_file(path)
        m
      end
    end
  end

  def node_hostnames
    @node_hostnames ||= begin
      self.nodes.map { |(n, c)| c['hostname'] }
    end
  end

  def repo_root
    @repo_root ||= begin
      File.expand_path('repos', self.node_root)
    end
  end

  def checkout_root
    @checkout_root ||= begin
      File.expand_path('checkouts', self.node_root)
    end
  end

  def reset!
    @nodes = @node_hostnames = @repo_root = @checkout_root = @local_node_bootstrapper_config = @local_node = @port = @config_repo_path = nil
  end

  def config
    self
  end

end