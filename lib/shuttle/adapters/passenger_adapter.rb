
module Shuttle
  module Adapters # :nodoc:
    class PassengerAdapter < Shuttle::Adapter
      
      on_install   :restart
      on_link      :restart
      
      def restart
        system.as_user(system.web_user, system.web_group) do
          tmp_restart = File.join(system.satellite_root, 'tmp', 'restart.txt')
          FileUtils.touch tmp_restart, :verbose => true
        end
      end
      
    end
  end
end
