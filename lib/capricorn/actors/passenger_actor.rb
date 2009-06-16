
module Capricorn
  module Actors # :nodoc:
    class PassengerActor < Capricorn::Actor
      
      after_install_satellite   :restart
      after_link_satellite      :restart
      
      # restart the current satellite.
      def restart
        system.as_user(system.web_user, system.web_group) do
          tmp_restart = system.passenger_restart_txt
          FileUtils.touch tmp_restart, :verbose => true
        end
      end
      
      module Config
        
        # set the passneger restart file.
        def passenger_restart_txt(&block)
          satellite_option(:passenger_restart_txt, block) { |s, v| v || File.join(satellite_root, 'tmp', 'restart.txt') }
        end
        
      end
      
    end
  end
end
