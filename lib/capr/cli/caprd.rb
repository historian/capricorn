class Capr::CLI::Caprd
  include Opts::DSL
  include Capr::Helpers::Shared

  class_use Opts::Shell
  # class_use Opts::ErrorHandler
  class_use Opts::Environment, 'CAPRD_'
  class_use Opts::ManHelp,
    :path    => File.expand_path('../../../man', __FILE__),
    :default => 'caprd.1'

  option 'root', :type => :string, :required => true
  def run(env, args)
    root = File.expand_path(env['ROOT'])

    Capr::Node.run(root)
  end

  option 'name', :type => :string, :required => true
  option 'root', :type => :string, :required => true
  option 'repo', :type => :string, :required => true
  def setup(env, args)
    root = File.expand_path(env['ROOT'])
    name = env['NAME']
    repo = env['REPO']

    FileUtils.mkdir_p(root)

    cmd = ["git", "clone", repo, File.join(root, 'config')]
    cmd = Shellwords.join(cmd)
    puts cmd
    system(cmd)

    File.open(File.join(root, '.config'), 'w+', 0644) do |f|
      f.puts name
      f.puts repo
    end
  end

end