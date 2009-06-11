
desc "build the docs"
task :doc do
  FileUtils.rm_rf('doc')
  system('hanna --title="Capricorn" --main=README.rdoc lib/**/* README.rdoc')
end

namespace :doc do
  desc 'send the docs to github'
  task :github => 'doc' do
    id = rand(10_000)
    
    FileUtils.mv('doc', "/tmp/#{id}")
    
    system('git co gh-pages')
    old = Dir.glob('./*') - ['.git', '.gitignore']
    old.each { |p| FileUtils.rm_rf(p) }
    
    FileUtils.mv(Dir.glob("/tmp/#{id}/*"), './')
    system('git add .')
    system('git add -u')
    system('git ci -m "Updated the documentation."')
    
    FileUtils.rm_rf("/tmp/#{id}")
    system('git co master')
  end
end

begin
  require 'flog'
  require 'flay'
  require 'roodi'
  require 'rake/tasklib'
  require 'roodi_task'
  require 'metric_fu'
  
  desc "Analyze for code complexity"
  task :flog do
    dirs = ['app', 'lib'].select { |d| File.directory? d }
    
    flog = Flog.new
    flog.flog_files dirs
    threshold = 40
  
    bad_methods = flog.totals.select do |name, score|
      score > threshold
    end
    bad_methods.sort { |a,b| a[1] <=> b[1] }.each do |name, score|
      puts "%8.1f: %s" % [score, name]
    end
    
    raise "#{bad_methods.size} methods have a flog complexity > #{threshold}" unless bad_methods.empty?
  end
  
  desc "Analyze for code duplication"
  task :flay do
    dirs = ['app', 'lib'].select { |d| File.directory? d }
    
    threshold = 25
    flay = Flay.new({:fuzzy => false, :verbose => false, :mass => threshold})
    flay.process(*Flay.expand_dirs_to_files(dirs))
  
    flay.report
  
    raise "#{flay.masses.size} chunks of code have a duplicate mass > #{threshold}" unless flay.masses.empty?
  end
  
  dirs = ['app', 'lib'].select { |d| File.directory? d }.collect { |d| d+'/**/*.rb'}
  RoodiTask.new 'roodi', dirs, 'roodi.yml'
  
  task :quality => [:flog, :flay, :roodi, 'metrics:all']
rescue LoadError
  puts "please run: gem install flog flay reek roodi jscruggs-metric_fu"
  exit(1)
end