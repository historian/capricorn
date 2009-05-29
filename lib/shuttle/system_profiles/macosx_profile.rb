
module Shuttle
  module SystemProfiles # :nodoc:
    class MacosxProfile < Shuttle::SystemProfile # :nodoc:
      profile_name :macosx
      
      can_activate? do
        ::Config::CONFIG['target_os'] == 'darwin9'
      end
      
      bin_paths { Dir.glob('/usr/local/bin') + Dir.glob('/usr/bin') }
      
    end
  end
end
