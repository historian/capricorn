require 'etc'

# ruby boot.rb User Group Wdir 0 Cmd
# ruby boot.rb User Group Wdir 1 PidPath Stdout Stderr Cmd
uid       = ARGV.shift
gid       = ARGV.shift
wdir      = ARGV.shift
daemonize = ARGV.shift == "1"

if daemonize
  pid_path  = ARGV.shift
  stdout    = ARGV.shift
  stderr    = ARGV.shift
end

cmd = ARGV

begin
  gid = Etc.getpwnam(gid).uid
  Process::Sys.setgid(gid)
  
  uid = Etc.getpwnam(uid).uid
  Process::Sys.setuid(uid)
  
  File.umask 0000 
rescue Exception
  puts "setuid failed!"
  exit(1)
end

begin
  Dir.chdir(wdir)
rescue Exception
  puts "chdir failed!"
  exit(2)
end

if daemonize
  if pid = fork
    exit(0)
  end
  
  unless Process.setsid
    puts 'cannot detach from controlling terminal'
    exit(1)
  end
  
  trap 'SIGHUP', 'IGNORE'
  
  if pid = fork
    File.open(pid_path, 'w+', '644') { |f| f.write pid.to_s }
    exit(0)
  end
  
  begin
    STDIN.reopen  "/dev/null"
    STDOUT.reopen stdout
    STDERR.reopen stderr
  rescue Exception
    puts "redirect io failed!"
    exit(1)
  end
end

exec(*cmd)