#!/usr/bin/env ruby

require File.dirname(__FILE__)+'/../lib/driver'
require 'rubygems/format'

Erlang do |path|
  
  unless File.extname(path) == ".gem" and File.file?(path)
    error :not_found
  end
  
  begin
    format = Gem::Format.from_file_by_path(path)
  rescue Gem::Exception => e
    error :gem_error, e.message
  end
  
  spec = format.spec
  unless spec
    error :gem_error, "Invalid gem"
  end
  
  dependencies = spec.runtime_dependencies.collect do |dep|
    version_requirements = dep.version_requirements.as_list.collect do |req|
      op, version = *req.split(/\s+/, 2)
      BERT::Tuple[:requirement, op, version]
    end
    BERT::Tuple[:dependency, dep.name, version_requirements]
  end
  
  send BERT::Tuple[:gem, spec.name, spec.version.to_s, dependencies]
  
end
