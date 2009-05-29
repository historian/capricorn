
module Shuttle
  module SystemProfiles # :nodoc:
    class MacportsProfile < Shuttle::SystemProfile # :nodoc:
      profile_name :macports
      based_on :macosx
      
      can_activate? do
        File.directory? '/opt/local'
      end
      
      bin_paths { Dir.glob('/opt/local/bin') + Dir.glob('/opt/local/apache2/bin') }
      gem_paths { Dir.glob('/opt/local/lib/ruby/gems/1.8') }
      
    end
  end
end
