class Capr::Config
  
  include Capr::SharedHelpers
  
  attr_reader :node_name, :node_root, :config_repo, :port
  
  def initialize(node_name, node_root, config_repo, port)
    @node_name,                 @node_root, @config_repo, @port = \
     node_name, File.expand_path(node_root), config_repo,  port
  end
  
  def nodes
    @nodes ||= begin
      repo = branch_path_for_and_branch(self.config_repo, 'master')
      
      paths = Dir.glob(File.expand_path('*/_node.yml'), repo)
      
      paths.inject({}) do |m, path|
        name = File.basename(File.dirname(path))
        m[name] = YAML.load_file(path)
        m
      end
    end
  end
  
  def node_hostnames
    @node_hostnames ||= begin
      self.nodes.map { |n| n['hostname'] }
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
    @nodes = @node_hostnames = @repo_root = @checkout_root = nil
  end
  
  def config
    self
  end
  
end