require 'digest/sha1'

domain = application[:domain]

user       = 'www-data'

db_name    = application[:id].downcase.gsub(/[^a-z]+/, '_')
db_name    = "#{db_name[0,58]}_#{Digest::SHA1.hexdigest(application[:id])[0,5]}"
db_user    = application[:id].downcase.gsub(/[^a-z]+/, '_')
db_user    = "#{db_user[0,8]}_#{Digest::SHA1.hexdigest(application[:id])[0,5]}"
db_pswd    = (rand(1_000_000_000) + 10_000).to_s

httpdocs_path = "/var/www/vhosts/#{domain}/httpdocs"
application_root = box["/var/www/vhosts/#{domain}/"]
FileUtils.mkdir_p(application_root.full_path)
has_db = application_root['shared/settings/database.yml'].exists?

###############################################################################
### Create host app                                                         ###
###############################################################################
if application_root['host/'].exists?
  application_root.bash %{ mv host host-#{Time.now.strftime('%Y%m%d%H%M%S')} }
end
application_root.bash %{ milkshake create:host "host" --git-data --shared-data "shared" }

unless has_db
  ###############################################################################
### Create Database                                                         ###
###############################################################################
  begin
    box.bash(%{ mysql --user=root --password=Kendydig4488 --execute="CREATE DATABASE #{db_name} ; GRANT ALL ON #{db_name}.* TO '#{db_user}'@'%' IDENTIFIED BY  '#{db_pswd}' ;" })
  rescue Rush::BashFailed
    # ignore failure
  end

  ###############################################################################
### Configure Database                                                      ###
###############################################################################
  database_conf = application_root['shared/settings/database.yml'].create
  database_conf.write %{
  default: &default
    adapter: mysql
    database: #{db_name}
    username: #{db_user}
    password: #{db_pswd}
    host: localhost
    encoding: utf8
    socket: /var/run/mysqld/mysqld.sock

  development:
    <<: *default

  test:
    <<: *default

  staging:
    <<: *default

  production:
    <<: *default
  }
end

###############################################################################
### Configure VHost                                                         ###
###############################################################################
vhost_conf = box['/etc/apache2/sites-available/'+domain].create
vhost_conf.write %[
<VirtualHost *:80>
  ServerName #{application[:domain]}
  #{application[:aliases].collect {|d| 'ServerAlias '+d+"\n  "}}

  RailsEnv #{application[:environment]}

  DocumentRoot "#{application_root.full_path}/host/public"

  ErrorDocument 503 /503.html
  RewriteEngine on
  RewriteCond %{DOCUMENT_ROOT}/../tmp/stop.txt -f
  RewriteCond %{DOCUMENT_ROOT}/%{REQUEST_FILENAME} !-f
  RewriteRule ^(.*)$ /$1 [R=503,L]

  <Directory   "#{application_root.full_path}/host/public">
    Options All
    AllowOverride All
    Order allow,deny
    Allow from all
  </Directory>
</VirtualHost>
]

###############################################################################
### Set Owner of Host                                                       ###
###############################################################################
box.bash %{ chown    #{user}:#{user} "#{application_root.full_path}" }
box.bash %{ chown -R #{user}:#{user} "#{application_root.full_path}/host" }
box.bash %{ chown -R #{user}:#{user} "#{application_root.full_path}/shared" }

###############################################################################
### Reconfigure vhost in Plesk                                              ###
###############################################################################
box.bash %{ /usr/sbin/a2ensite #{domain} }

###############################################################################
### Restart apache                                                          ###
###############################################################################
box.bash %{ /etc/init.d/apache2 restart }

set :root_path, application_root.full_path.to_s
set :www_user,  user.to_s
set :www_group, user.to_s

