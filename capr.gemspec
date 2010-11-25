# -*- encoding: utf-8 -*-
require File.expand_path("../lib/capr/version", __FILE__)

Gem::Specification.new do |s|
  s.name        = "capr"
  s.version     = Capr::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = []
  s.email       = []
  s.homepage    = "http://rubygems.org/gems/capr"
  s.summary     = "TODO: Write a gem summary"
  s.description = "TODO: Write a gem description"

  s.required_rubygems_version = ">= 1.3.6"
  s.rubyforge_project         = "capr"

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
