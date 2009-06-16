
module Capricorn
  module Actors # :nodoc:
    class PleskActor < Capricorn::Actor
      
      before_install_satellite   :create_client
      before_install_satellite   :create_domain
      after_install_satellite    :link_htdocs
      after_install_satellite    :reload_subdomains
      after_install_satellite    :restart_apache
      on_install_satellite       :create_database
      on_uninstall_satellite     :drop_databases
      
      def create_client
        # only do this if this is needed
        if false
          unless system.client?(system.plesk_client)
            system.create_client(system.plesk_client, system.plesk_client, 'mypwd')
          end
        end
      end
      
      def create_domain
        unless system.domain?(satellite.basedomain)
          system.create_domain(system.plesk_client, satellite.basedomain,
            system.web_user[0,20], (rand(1_000_000_000) + 10_000).to_s)
        else
          httpdocs_path = "/var/www/vhosts/#{satellite.basedomain}/httpdocs"
          system.set_satellite_option(:web_user,
            system.get_user_name(File.stat(httpdocs_path).uid))
        end
          
        if satellite.subdomain? and !system.subdomain?(satellite.basedomain, satellite.subdomain)
          system.create_subdomain(satellite.basedomain, satellite.subdomain)
        end
        
        FileUtils.mkdir_p(system.satellite_root)
        FileUtils.mkdir_p(system.shared_root)
        FileUtils.chown_R(system.web_user, system.web_group, system.satellite_root)
        FileUtils.chown_R(system.web_user, system.web_group, system.shared_root)
      end
      
      def link_htdocs
        httpdocs_path = File.join(File.dirname(system.satellite_root), 'httpdocs')
        FileUtils.rm_rf(httpdocs_path)
        FileUtils.ln_s(system.satellite_root, httpdocs_path)
        FileUtils.chown_R(system.web_user, system.web_group, httpdocs_path)
        
        File.open(File.join(system.satellite_root, '../conf/vhost.conf'), 'w+') do |f|
          f.write %{DocumentRoot #{httpdocs_path}/public
<Directory  "#{httpdocs_path}/public">
  Options All
  AllowOverride All
  Order allow,deny
  Allow from all
</Directory>
}
        end
      end
      
      def reload_subdomains
        system.run("#{system.plesk_websrvmng_bin} --reconfigure-vhost --vhost-name=#{satellite.basedomain}")
      end
      
      def restart_apache
        system.run("#{system.plesk_httpd_bin} restart")
      end
      
      def create_database
        db_name = database_name_for(satellite)
        db_user = db_name[0,13]
        db_pswd = (rand(1_000_000_000) + 10_000).to_s
        
        create_database_for(satellite, :d, db_name, db_user, db_pswd)
        create_database_for(satellite, :t, db_name, db_user, db_pswd)
        create_database_for(satellite, :p, db_name, db_user, db_pswd)
        
        system.as_user(system.web_user, system.web_group) do
          db_file = File.join(system.satellite_root, 'config', 'database.yml')
          write_database_config(db_file, db_name, db_user, db_pswd)
        end
      end
      
      def drop_databases
        db_name = satellite.domain.downcase.gsub(/[^a-z]+/, '_')[0,63]
        
        system.drop_database("#{db_name}_d")
        system.drop_database("#{db_name}_t")
        system.drop_database("#{db_name}_p")
      end
      
      def write_database_config(db_file, db_name, db_user, db_pswd)
        File.open(db_file, 'w+') { |f| f.write(%{
development:
  adapter: mysql
  database: #{db_name}_d
  username: #{db_user}_ud
  password: #{db_pswd}
  host: localhost
  encoding: utf8
  socket: /var/lib/mysql/mysql.sock

test:
  adapter: mysql
  database: #{db_name}_t
  username: #{db_user}_ut
  password: #{db_pswd}
  host: localhost
  encoding: utf8
  socket: /var/lib/mysql/mysql.sock

production:
  adapter: mysql
  database: #{db_name}_p
  username: #{db_user}_up
  password: #{db_pswd}
  host: localhost
  encoding: utf8
  socket: /var/lib/mysql/mysql.sock
}) }
      end
      
      def database_name_for(satellite)
        satellite.domain.downcase.gsub(/[^a-z]+/, '_')[0,63]
      end
      
      def create_database_for(satellite, environment, db_name, db_user, db_pswd)
        db_name = "#{db_name}_#{environment}"
        db_user = "#{db_user}_u#{environment}"
        system.create_database(satellite.basedomain, db_name, db_user, db_pswd)
      end
      
      module Helper
        
        def create_client(name, login, pwd)
          
        end
        
        # check is the client exists
        def client?(login)
          !(run("#{plesk_client_bin} -i #{login}") =~ /Object not found: Client/)
        end
        
        # create a domain through plesk
        def create_domain(client, domain, user, passwd)
          run("#{plesk_domain_bin} -c #{domain} -clogin #{client} -status enabled -hosting true -hst_type phys -dns true -www true -login #{user} -passwd #{passwd} -shell /bin/bash")
        end
        
        #  check if a domain exists with plesk
        def domain?(domain)
          !(run("#{plesk_domain_bin} -i #{domain}") =~ /Object not found: Domain/)
        end
        
        # create a subdomain through plesk
        def create_subdomain(basedomain, subdomain)
          run("#{plesk_subdomain_bin} -c #{subdomain} -d #{basedomain}")
        end
        
        #  check if a subdomain exists with plesk
        def subdomain?(basedomain, subdomain)
          run("#{plesk_subdomain_bin} -i -s #{subdomain} -d #{basedomain}") =~ /SUCCESS: Gathering/
        end
        
        # create a database through plesk
        def create_database(basedomain, db_name, db_user, pwd)
          o = run("#{plesk_database_bin} -c #{db_name} -domain #{basedomain} -server #{plesk_database_server} -add_user #{db_user} -passwd #{pwd}")
          if o =~ /Database with requested name already exists/
            o = run("#{plesk_database_bin} -u #{db_name} -add_user #{db_user} -passwd #{pwd}")
            if o =~ /Unable to create database user: User/
              o = run("#{plesk_database_bin} -u #{db_name} -update_user #{db_user} -passwd #{pwd}")
            end
          end
        end
        
        # drop a database through plesk
        def drop_database(db_name)
          run("#{plesk_database_bin} -r #{db_name}")
        end
        
      end
      
      module Config
        
        # set the plesk client to be used for this satellite.
        def plesk_client(&block)
          satellite_option(:plesk_client, block)
        end
        
        # path to the client tool.
        def plesk_client_bin(&block)
          option(:plesk_client_bin, block) { |v| v or '/usr/local/psa/bin/client' }
        end
        
        # path to the domain tool.
        def plesk_domain_bin(&block)
          option(:plesk_domain_bin, block) { |v| v or '/usr/local/psa/bin/domain' }
        end
        
        # path to the subdomain tool.
        def plesk_subdomain_bin(&block)
          option(:plesk_subdomain_bin, block) { |v| v or '/usr/local/psa/bin/subdomain' }
        end
        
        # path to the database tool.
        def plesk_database_bin(&block)
          option(:plesk_database_bin, block) { |v| v or '/usr/local/psa/bin/database' }
        end
        
        # path to the websrvmng tool.
        def plesk_websrvmng_bin(&block)
          option(:plesk_websrvmng_bin, block) { |v| v or '/usr/local/psa/admin/sbin/websrvmng' }
        end
        
        # path to the httpd tool.
        def plesk_httpd_bin(&block)
          option(:plesk_httpd_bin, block) { |v| v or '/etc/init.d/httpd' }
        end
        
        # host and port of database server.
        def plesk_database_server(&block)
          option(:plesk_database_server, block)
        end
        
      end
      
    end
  end
end
