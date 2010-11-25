class Capr::CaprdCLI
  include Opts::DSL
  include Capr::SharedHelpers

  class_use Opts::Shell
  class_use Opts::ErrorHandler
  class_use Opts::Environment, 'CAPRD_'
  class_use Opts::ManHelp,
    :path    => File.expand_path('../../../man', __FILE__),
    :default => 'caprd.1'
  
  option 'root', :type => :string, :required => true
  def run(env, args)
    root = File.expand_path(env['ROOT'])
    name = env['NAME']
    
    Capr::Node.run(name, root, repo, port)
  end
  
  option 'name', :type => :string, :required => true
  option 'root', :type => :string, :required => true
  option 'repo', :type => :string, :required => true
  def init(env, args)
    root = File.expand_path(env['ROOT'])
    name = env['NAME']
    repo = env['REPO']
    
    FileUtils.mkdir_p(root)
    repo_dir = File.expand_path('repos/'+dirname_for_url(repo), root)
    work_dir = File.expand_path('checkouts/'+dirname_for_url_and_branch(repo, 'master'), root)
    
    FileUtils.mkdir_p(File.dirname(repo_dir))
    FileUtils.mkdir_p(work_dir)
    
    cmd = ["git", "clone", "--bare", repo, repo_dir]
    cmd = Shellwords.join(cmd)
    puts cmd
    system(cmd)
    
    cmd = ["git", "--git-dir", repo_dir, "--work-tree", work_dir, "reset", "--hard", "master"]
    cmd = Shellwords.join(cmd)
    puts cmd
    system(cmd)
    
    File.open(File.join(root, '.config'), 'w+', 0644) do |f|
      f.puts name
      f.puts repo
    end
  end
  
end