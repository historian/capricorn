
module Shuttle
  class Adapter
    
    attr_reader :satellite
    
    def initialize(satellite)
      @satellite = satellite
    end
    
    def system
      Shuttle.system
    end
    
    def run_install_callbacks!
      self.class.install_callbacks.each { |meth| send(meth) }
    end
    
    def run_uninstall_callbacks!
      self.class.uninstall_callbacks.each { |meth| send(meth) }
    end
    
    def run_link_callbacks!
      self.class.link_callbacks.each { |meth| send(meth) }
    end
    
    def switch_to_user(username, group=nil)
      old_uid, old_gid = Process.uid, Process.gid
      old_euid, old_egid = Process.euid, Process.egid
      Process.uid = Etc.getpwnam(username).uid
      Process.gid = Etc.getgrnam(group).gid if group
      Process.euid = Etc.getpwnam(username).uid
      Process.egid = Etc.getgrnam(group).gid if group
      yield
      Process.uid = old_uid
      Process.gid = old_gid if group
      Process.euid = old_euid
      Process.egid = old_egid if group
    end
    
    class << self
      
      def on_install(meth)
        self.install_callbacks.push(meth)
      end
      
      def on_uninstall(meth)
        self.uninstall_callbacks.push(meth)
      end
      
      def on_link(meth)
        self.link_callbacks.push(meth)
      end
      
      def install_callbacks
        @install_callbacks ||= []
      end
      
      def uninstall_callbacks
        @uninstall_callbacks ||= []
      end
      
      def link_callbacks
        @link_callbacks ||= []
      end
      
    end
    
  end
end