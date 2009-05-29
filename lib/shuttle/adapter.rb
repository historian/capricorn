
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