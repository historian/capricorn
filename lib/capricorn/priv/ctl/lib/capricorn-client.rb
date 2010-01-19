module Capricorn
  require 'thor'
  require 'bertrpc'
  require 'highline'
  
  require 'capricorn-client/helpers'
  
  module CLI
    require 'capricorn-client/cli/applications'
    require 'capricorn-client/cli/gems'
    require 'capricorn-client/cli/machines'
  end
end