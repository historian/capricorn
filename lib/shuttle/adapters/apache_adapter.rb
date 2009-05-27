
module Shuttle
  module Adapters
    class ApacheAdapter < Shuttle::Adapter
      
      on_install   :write_config_file
      on_install   :restart
      
      on_uninstall :remove_config_file
      on_uninstall :restart
      
      def restart
        %x[#{system.apachectl_path} -k restart]
      end
      
      def write_config_file
        config = %{<VirtualHost *>
  ServerName #{satellite.domain}
  
  ErrorLog   logs/#{satellite.domain}.error.log
  CustomLog  logs/#{satellite.domain}.access.log common
  DocumentRoot #{system.satellite_root}/public
  <Directory "#{system.satellite_root}/public">
    Options All
    AllowOverride All
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>}
        File.open(system.apache_conf_path, 'w+') { |f| f.write config }
      end
      
      def remove_config_file
        File.unlink(system.apache_conf_path) if File.exist? system.apache_conf_path
      end
      
      module Macros
        
        def apachectl_path(&block)
          option(:apachectl_path, block)
        end
        
        def apache_conf_path(&block)
          option(:apache_conf_path, block)
        end
        
      end
      
    end
  end
end
