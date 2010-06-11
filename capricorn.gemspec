# -*- encoding: utf-8 -*-
lib = File.expand_path('../lib/', __FILE__)
$:.unshift lib unless $:.include?(lib)

require 'capricorn/version'

Gem::Specification.new do |s|
  s.name        = "capricorn"
  s.version     = Capricorn::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ["Simon Menke"]
  s.email       = ["simon.menke@gmail.com"]
  s.homepage    = "http://github.com/fd/capricorn"
  s.summary     = "Build, manage and configure many railsish web applications"
  s.description = "Build, manage and configure many railsish web applications."

  s.required_rubygems_version = ">= 1.3.6"
  s.rubyforge_project         = "capricorn"

  s.require_path = 'lib'
  s.files        = Dir.glob("{lib,erlang,ext}/**/*") +
                   %w(LICENSE README.md ext/Makefile )

  s.executables = %w( capricornctl capricornd capricorn-app-scaffolder capricorn-gem-spec )
  s.extensions  = %w( ext/extconf.rb )

  s.add_runtime_dependency 'fd-bertrpc',            '= 1.3.1'
  s.add_runtime_dependency 'mustache',              '= 0.5.1'
  s.add_runtime_dependency 'rush',                  '= 0.6.5'
  s.add_runtime_dependency 'session',               '= 3.1.0'
  s.add_runtime_dependency 'thor',                  '= 0.13.6'
  s.add_runtime_dependency 'public_suffix_service', '= 0.4.0'
end