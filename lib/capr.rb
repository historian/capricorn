module Capr

  require 'eventmachine'
  require 'cramp/controller'
  require 'yajl'
  require 'em-http-request'
  require 'usher'
  require 'opts'
  require 'uri'
  require 'yaml'
  require 'shellwords'
  
  require 'capr/version'

  module Do
    require 'capr/do'
  end
  
  module Helpers
    require 'capr/helpers/shared'
    require 'capr/helpers/config'
    require 'capr/helpers/shell'
  end

  module Git
    require 'capr/git/pull'
    require 'capr/git/clone'
    require 'capr/git/fetch'
    require 'capr/git/rebase'
    require 'capr/git/record_refs'
    require 'capr/git/reset_refs'
  end
  
  module Httpc
    require 'capr/httpc/forward'
    require 'capr/httpc/ping'
  end
  
  module Httpd
    require 'capr/httpd/forward'
    require 'capr/httpd/proxy_ping'
    require 'capr/httpd/update_config'
  end

  module CGI
    require 'capr/cgi/forward'
    require 'capr/cgi/ping'
  end

  module CLI
    require 'capr/cli/caprd'
    require 'capr/cli/capr'
  end
  
  require 'capr/config'
  require 'capr/node'

end
