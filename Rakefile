
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