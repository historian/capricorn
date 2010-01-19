#!/usr/bin/env ruby

require 'etc'
require File.dirname(__FILE__)+'/../lib/driver'

uid = Etc.getpwnam(ARGV[1] || "nobody").uid
Process::Sys.setuid(uid)

ENV["RAILS_ENV"] = (ARGV[0] || "development").dup

require File.expand_path('./config/environment')

Erlang do |cmd|
  begin
    if defined?(Capricorn) and defined?(Capricorn::Services)
      Capricorn::Services.dispatch(self, cmd)
    else
      send t[:error, :no_services]
    end
  ensure
    Rails.logger.flush if defined?(Rails) and Rails.logger.respond_to?(:flush)
  end
end
