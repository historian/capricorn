
desc "build the capricorn"
task :build do
  sh "gem build capricorn.gemspec"
  system("mkdir -p pkg ; mv ./*.gem pkg/")
end

desc "install the capricorn"
task :install => [:load_version, :build] do
  sh "gem install -l pkg/capricorn-#{Capricorn::VERSION}.gem"
end

desc "release the capricorn"
task :release => [:load_version, :build] do
  unless %x[ git status 2>&1 ].include?('nothing to commit (working directory clean)')
    puts "Your git stage is not clean!"
    exit(1)
  end

  if %x[ git tag 2>&1 ] =~ /^#{Regexp.escape(Capricorn::VERSION)}$/
    puts "Please bump your version first!"
    exit(1)
  end

  require File.expand_path('../lib/capricorn/version', __FILE__)
  sh "gem push pkg/capricorn-#{Capricorn::VERSION}.gem"
  sh "git tag -a -m \"#{Capricorn::VERSION}\" #{Capricorn::VERSION}"
  sh "git push origin master"
  sh "git push origin master --tags"
end

desc 'Build the manual'
task :man => :load_version do
  require 'ronn'
  ENV['RONN_MANUAL']  = "Capricorn #{Capricorn::VERSION}"
  ENV['RONN_ORGANIZATION'] = "Simon Menke"
  sh "ronn -w -s toc man/*.ronn"
end

desc 'Build website'
task :site => :man do
  repo = Dir.pwd
  
  begin
    tmp = FileUtils.mkdir_p("/tmp/capr-site-#{Time.now.to_i}")
    
    sh "git clone #{repo}/.git #{tmp}"
    
    Dir.chdir(tmp) do
      sh "git checkout gh-pages"
      sh "rm ./*.html"
      sh "cp #{File.join(repo, 'man/*.html')} ./"
      sh "cp capricorn.7.html index.html"
      sh "git add . && git add -u"
      unless %x[git status].include?('nothing to commit')
        sh "git commit -m \"Updated website\""
        sh "git push origin gh-pages"
      end
    end
    
  ensure
    FileUtils.rm_rf(tmp)
  end
end

begin
  require 'yard'
  YARD::Rake::YardocTask.new do |t|
    t.files   = FileList['lib/**/*.rb'].to_a
    t.options = ['-m', 'markdown', '--files', FileList['documentation/*.markdown'].to_a.join(',')]
  end
rescue LoadError
  puts "YARD not available. Install it with: sudo gem install yard"
end

require 'rake/testtask'
Rake::TestTask.new(:test) do |test|
  test.libs << 'test'
  test.pattern = 'test/**/*_test.rb'
  test.verbose = true
end

begin
  require 'rcov/rcovtask'
  Rcov::RcovTask.new do |test|
    test.libs << 'test'
    test.pattern = 'test/**/*_test.rb'
    test.verbose = true
  end
rescue LoadError
  task :rcov do
    abort "RCov is not available. In order to run rcov, you must: sudo gem install spicycode-rcov"
  end
end

task :load_version do
  require File.expand_path('../lib/capricorn/version', __FILE__)
end