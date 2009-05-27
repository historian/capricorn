require 'rubygems'
require 'thor'
require 'daemons'

module Shuttle
  autoload :App,    File.dirname(__FILE__)+'/shuttle/app'
  autoload :Server, File.dirname(__FILE__)+'/shuttle/server'
end