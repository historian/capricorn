
# find my home
home = [box['/Users/Simon/'], box['/Users/simon/']]
home = home.find { |p| p.exists? }
username = home.name

# create the capricorn root
capricorn_root = home['/capricorn/'].create
home.bash %{ chown #{username}:staff "#{capricorn_root.full_path}" }

# create the host app
application_root = capricorn_root[application[:id]+'/'].create
if application_root['host/'].exists?
  application_root.bash %{ rm -rf host }
end
application_root.bash %{ milkshake create.host "host" --git-data --shared-data "shared" }
capricorn_root.bash %{ chown -R #{username}:staff "#{application_root.full_path}" }

# create the vhost
apache_conf = box['/opt/local/apache2/conf/apps/']
vhost_conf  = apache_conf[application[:id] + ".conf"].create
vhost_conf.write %{
<VirtualHost *>
  ServerName #{application[:domain]}
  #{application[:aliases].collect {|d| 'ServerAlias '+d+"\n  "}}
  
  RailsEnv #{application[:environment]}
  
  ErrorLog   logs/#{application[:id]}.error.log
  CustomLog  logs/#{application[:id]}.access.log common
  DocumentRoot "#{application_root.full_path}/host/public"
  
  <Directory   "#{application_root.full_path}/host/public">
    Options All
    AllowOverride All
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>
}

# restart apache
box.bash %{ /opt/local/apache2/bin/apachectl -k restart }

# export variables
set :root_path, application_root.full_path.to_s
set :www_user,  username.to_s
set :www_group, 'staff'
