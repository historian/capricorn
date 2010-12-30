#!/usr/bin/env ruby
$stdout.sync = true

puts "Waithing for lock"
system "capr-lock hello"

puts "hello #{ENV['CAPR_HELLO']}"
sleep 5
puts "bye"