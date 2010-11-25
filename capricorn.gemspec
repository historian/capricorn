# -*- encoding: utf-8 -*-
require File.expand_path("../lib/capr/version", __FILE__)

Gem::Specification.new do |s|
  s.name        = "capricorn"
  s.version     = Capr::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ['Simon Menke']
  s.email       = ['simon.menke@gmail.com']
  s.homepage    = "http://rubygems.org/gems/capricorn"
  s.summary     = "Capricorn is a Rails deployment platform"
  s.description = "...---..."

  s.required_rubygems_version = ">= 1.3.6"
  s.rubyforge_project         = "capricorn"

  s.add_runtime_dependency 'eventmachine'
  s.add_runtime_dependency 'cramp'
  s.add_runtime_dependency 'yajl-ruby'
  s.add_runtime_dependency 'em-http-request'
  s.add_runtime_dependency 'usher'
  s.add_runtime_dependency 'opts'
  s.add_development_dependency "bundler", ">= 1.0.0"

  s.files        = `git ls-files`.split("\n")
  s.executables  = `git ls-files`.split("\n").map{|f| f =~ /^bin\/(.*)/ ? $1 : nil}.compact
  s.require_path = 'lib'
end
