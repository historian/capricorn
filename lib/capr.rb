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
  
  require 'capr/helpers/shared_helpers'
  require 'capr/helpers/config_helpers'
  require 'capr/helpers/shell_helpers'

  require 'capr/config'
  require 'capr/node'

  require 'capr/actions/forwarder'
  
  autoload :CaprdCLI, 'capr/caprd_cli'

end
