
module Shuttle
  module Actors # :nodoc:
    class PassengerActor < Shuttle::Actor
      
      after_install_satellite   :restart
      after_link_satellite      :restart
      
      # restart the current satellite.
      def restart
        system.as_user(system.web_user, system.web_group) do
          tmp_restart = File.join(system.satellite_root, 'tmp', 'restart.txt')
          FileUtils.touch tmp_restart, :verbose => true
        end
      end
      
    end
  end
end
