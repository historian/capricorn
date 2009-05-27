
module Shuttle
  module Adapters
    class PassengerAdapter < Shuttle::Adapter
      
      on_install   :restart
      on_link      :restart
      
      def restart
        switch_to_user(system.web_user, system.web_group) do
          tmp_restart = File.join(system.satellite_root, 'tmp', 'restart.txt')
          FileUtils.touch tmp_restart
        end
      end
      
    end
  end
end
