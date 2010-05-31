require 'digest/sha1'

basedomain = application[:domain].split('.')[-2, 2].join('.')
subdomain  = application[:domain].split('.')[0..-3].join('.')
subdomain  = nil if subdomain.nil? or subdomain.empty?

client     = 'mh1179'

user       = application[:id].downcase.gsub(/[^a-z]+/, '_')
short_user = "#{user[0,14]}_#{Digest::SHA1.hexdigest(application[:id])[0,5]}"
passwd     = (rand(1_000_000_000) + 10_000).to_s

db_name    = application[:id].downcase.gsub(/[^a-z]+/, '_')
db_name    = "#{db_name[0,58]}_#{Digest::SHA1.hexdigest(application[:id])[0,5]}"
db_user    = application[:id].downcase.gsub(/[^a-z]+/, '_')
db_user    = "#{db_user[0,8]}_#{Digest::SHA1.hexdigest(application[:id])[0,5]}"
db_pswd    = (rand(1_000_000_000) + 10_000).to_s

###############################################################################
### Find Unused Username                                                    ###
###############################################################################
begin
  Etc.getpwnam(short_user)
rescue ArgumentError
  short_user = short_user.succ
  # retry
end

###############################################################################
### Create Plesk Domain                                                     ###
###############################################################################
begin
  ### Check if the domain exists
  box.bash(%{ /usr/local/psa/bin/domain -i #{basedomain} })
  httpdocs_path = "/var/www/vhosts/#{basedomain}/httpdocs"
  user = Etc.getpwuid(File.stat(httpdocs_path).uid).name
  short_user = user
  
rescue Rush::BashFailed, Errno::ENOENT
  ### try to create the base domain
  box.bash(%{ /usr/local/psa/bin/domain -c #{basedomain} -clogin #{client} -status enabled -hosting true -hst_type phys -dns true -www true -login #{short_user} -passwd #{passwd} -shell /bin/bash })
end
application_root = box["/var/www/vhosts/#{basedomain}/"]

###############################################################################
### Create Plesk Subdomain                                                  ###
###############################################################################
if subdomain
  begin
    ### check is the subdomain exists
    box.bash(%{ /usr/local/psa/bin/subdomain -i -s #{subdomain} -d #{basedomain} })
    
  rescue Rush::BashFailed
    ### try to create the subdomain
    box.bash(%{ /usr/local/psa/bin/subdomain -c #{subdomain} -d #{basedomain} })
  end
  application_root = application_root["subdomains/#{subdomain}/"]
end

###############################################################################
### Add Domain Aliasses                                                     ###
###############################################################################
unless subdomain
  application[:aliases].each do |aliasd|
    begin
      box.bash(%{ /usr/local/psa/bin/domalias --create #{aliasd} -domain #{basedomain} -status enabled -mail true -web true -dns true })
    rescue Rush::BashFailed
      # ignore failed aliases
    end
  end
end

has_db = application_root['shared/settings/database.yml'].exists?

###############################################################################
### Create host app                                                         ###
###############################################################################
if application_root['host/'].exists?
  application_root.bash %{ mv host host-#{Time.now.strftime('%Y%m%d%H%M%S')} }
end
application_root.bash %{ milkshake create.host "host" --git-data --shared-data "shared" }

unless has_db
  ###############################################################################
  ### Create Database                                                         ###
  ###############################################################################
  begin
    box.bash(%{ /usr/local/psa/bin/database -c #{db_name} -domain #{basedomain} -server localhost:3306 -add_user #{db_user} -passwd #{db_pswd} })
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
    socket: /var/lib/mysql/mysql.sock
  
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
vhost_conf = application_root['conf/vhost.conf'].create
vhost_conf.write %{
DocumentRoot #{application_root.full_path}/host/public
RailsEnv     #{application[:environment] || 'production'}

ErrorDocument 503 /503.html
RewriteEngine on
RewriteCond %{DOCUMENT_ROOT}/../tmp/stop.txt -f
RewriteCond %{DOCUMENT_ROOT}/%{REQUEST_FILENAME} !-f
RewriteRule ^(.*)$ /$1 [R=503,L]

<Directory  "#{application_root.full_path}/host/public">
  Options All
  AllowOverride All
  Order allow,deny
  Allow from all
</Directory>
}

###############################################################################
### Set Owner of Host                                                       ###
###############################################################################
box.bash %{ chown    #{short_user}:psaserv "#{application_root.full_path}" }
box.bash %{ chown -R #{short_user}:psaserv "#{application_root.full_path}/host" }
box.bash %{ chown -R #{short_user}:psaserv "#{application_root.full_path}/shared" }

###############################################################################
### Reconfigure vhost in Plesk                                              ###
###############################################################################
box.bash %{ /usr/local/psa/admin/sbin/websrvmng --reconfigure-vhost --vhost-name=#{basedomain} }

###############################################################################
### Restart apache                                                          ###
###############################################################################
box.bash %{ /etc/init.d/httpd restart }

set :root_path, application_root.full_path.to_s
set :www_user,  short_user.to_s
set :www_group, 'psaserv'
