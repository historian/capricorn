use :PassengerActor
use :ApacheActor
use :MysqlActor

apachectl_path { '/opt/local/apache2/bin/apachectl' }
ruby_path      { '/opt/local/bin/ruby' }
gem_path       { '/opt/local/bin/gem' }


web_user  { 'www' }
web_group { 'www' }

satellite_root { |s| "/opt/local/var/www/#{s.domain}/satellite" }
shared_root    { |s| "/opt/local/var/www/#{s.domain}/shared"    }
