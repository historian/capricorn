use :PassengerAdapter
use :PleskAdapter

apachectl_path { '/opt/local/apache2/bin/apachectl' }
ruby_path      { '/usr/bin/ruby' }
gem_path       { '/usr/bin/gem' }
plesk_path     { '/usr/bin/gem' }

web_user  { |s| s.domain.gsub(/[^a-zA-Z0-9]+/, '_') }
web_group { 'psacln' }

plesk_root { |s| "/var/www/vhost" }

satellite_root do |s|
  if s.subdomain?
    "#{plesk_root}/#{s.basedomain}/subdomains/#{s.subdomain}/satellite"
  else
    "#{plesk_root}/#{s.domain}/satellite"
  end
end

shared_root do |s|
  if s.subdomain?
    "#{plesk_root}/#{s.basedomain}/subdomains/#{s.subdomain}/shared"
  else
    "#{plesk_root}/#{s.domain}/shared"
  end
end