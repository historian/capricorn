
module Capricorn
  module Daemon
    class Base
      
      def self.daemonize(&block)
        Capricorn::Daemon::Controller.start(self,&block)
      end
      
    end
  
    module PidFile
      def self.store(pid)
        File.open(self.pid_file, 'w') {|f| f << pid}
      end
  
      def self.recall
        IO.read(self.pid_file).to_i rescue nil
      end
      
      def self.destroy
        FileUtils.rm(self.pid_file) if self.exist?
      end
      
      def self.pid_file
        Capricorn.system.path("Server.pid")
      end
      
      def self.exist?
        File.file?(self.pid_file)
      end
    end
  
    module Controller
      
      def self.start(daemon, &block)
        fork do
          Process.setsid
          exit if fork
          if PidFile.exist?
            puts "Pid file #{PidFile.pid_file} already exists. Not starting."
            exit 1
          end
          
          setup_child
          regitser_handlers(daemon, &block)
          
          daemon.start
        end
        puts "Daemon started."
      end
      
      def self.stop
        if !Capricorn::Daemon::PidFile.exist?
          puts "Pid file not found. Is the daemon started?"
          exit
        end
        pid = Capricorn::Daemon::PidFile.recall
        pid && Process.kill("TERM", pid)
      rescue Errno::ESRCH
        puts "Pid file found, but process was not running. The daemon may have died."
      end
      
    private
      
      def self.setup_child
        Capricorn::Daemon::PidFile.store(Process.pid)
        Dir.chdir Capricorn.system.root
        File.umask 0000
        Capricorn::ExceptionHandler.redirect_std
      end
      
      def self.regitser_handlers(daemon, &block)
        trap("TERM") { daemon.stop; exit }
        at_exit { Capricorn::Daemon::PidFile.destroy if $master }
        at_exit(&block) if block
      end
      
    end
  end
end
