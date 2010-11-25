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

  s.add_runtime_dependency     'eventmachine',    "~> 0.12"
  s.add_runtime_dependency     'cramp',           "~> 0.11"
  s.add_runtime_dependency     'yajl-ruby',       "~> 0.7"
  s.add_runtime_dependency     'em-http-request', "~> 0.2"
  s.add_runtime_dependency     'usher',           "~> 0.8"
  s.add_runtime_dependency     'opts',            "~> 0.0"
  s.add_development_dependency 'bundler',         "~> 1.0"
  s.add_development_dependency 'ronn',            "~> 0.7"

  s.files        = `git ls-files`.split("\n")
  s.executables  = `git ls-files`.split("\n").map{|f| f =~ /^bin\/(.*)/ ? $1 : nil}.compact
  s.require_path = 'lib'
end
