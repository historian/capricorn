
module Shuttle
  class SystemProfile
    
    def self.can_activate?(&block)
      if block
        @can_activate_proc = block
      else
        if @can_activate.nil?
          @can_activate = (TrueClass === @can_activate_proc.call)
          if @can_activate && self.parent_profile
            @can_activate = self.parent_profile.can_activate? and @can_activate
          end
        end
        @can_activate
      end
    end
    
    def self.depth
      unless @depth
        if self.parent_profile
          @depth = self.parent_profile.depth + 1
        else
          @depth = 0
        end
      end
      @depth
    end
    
    def self.profile_name(name=nil)
      if name
        @profile_name = name.to_sym
        Shuttle::SystemProfile.profiles[@profile_name] = self
      end
      @profile_name
    end
    
    def self.parent_profile
      @parent_profile = Shuttle::SystemProfile.profiles[@parent] if @parent
      @parent_profile
    end
    
    def self.parent_profile_name
      @parent
    end
    
    def self.active_profile
      unless @active_profile
        Dir.glob(File.dirname(__FILE__)+'/system_profiles/*_profile.rb').each do |path|
          require path
        end
        
        profiles = Shuttle::SystemProfile.profiles.values
        profiles = profiles.select { |profile| profile.can_activate? }
        profiles = profiles.sort { |a,b| b.depth <=> a.depth }
        @active_profile = profiles.first
      end
      @active_profile
    end
    
    def self.profiles
      @profiles ||= {}
    end
    
    def self.active_profiles
      @active_profiles ||= {}
    end
    
    def self.based_on(parent)
      @parent = parent.to_sym
    end
    
    def self.bin_paths(&block)
      if block
        @bin_paths_proc = block
      else
        unless @bin_paths
          @bin_paths = []
          if @bin_paths_proc
            @bin_paths.concat [@bin_paths_proc.call].flatten.compact
          end
          if self.parent_profile
            @bin_paths.concat self.parent_profile.bin_paths
          end
        end
        @bin_paths
      end
    end
    
    def self.gem_paths(&block)
      if block
        @gem_paths_proc = block
      else
        unless @gem_paths
          @gem_paths = []
          if @gem_paths_proc
            @gem_paths.concat [@gem_paths_proc.call].flatten.compact
          end
          if self.parent_profile
            @gem_paths.concat self.parent_profile.gem_paths
          end
        end
        @gem_paths
      end
    end
    
  end
end
