
desc "build and publish the docs"
task :doc do
  id = rand(10_000)
  
  FileUtils.rm_rf('doc')
  system('hanna lib/**/*')
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
