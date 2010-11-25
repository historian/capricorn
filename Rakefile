require 'bundler'
Bundler::GemHelper.install_tasks

desc 'Build the manual'
task :man => :load_version do
  ENV['RONN_MANUAL']  = "Capricorn #{Capr::VERSION}"
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

task :load_version do
  require File.expand_path('../lib/capr/version', __FILE__)
end
