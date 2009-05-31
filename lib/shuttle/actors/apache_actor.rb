
module Shuttle
  module Actors # :nodoc:
    class ApacheActor < Shuttle::Actor
      
      on_install_satellite :write_config_file
      after_install_satellite :restart
      
      on_uninstall_satellite :remove_config_file
      after_uninstall_satellite :restart
      
      def restart
        system.run "#{system.apachectl_path} -k restart"
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
          option(:apachectl_path, block) { |v| v or find_bin('apache2ctl', 'apachectl') }
        end
        
        def apache_conf_path(&block)
          satellite_option(:apache_conf_path, block) { |s,v| v or "/opt/local/apache2/conf/apps/#{s.domain}.conf" }
        end
        
      end
      
    end
  end
end
