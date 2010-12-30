#!/usr/bin/env ruby

# puts "helloW"
# puts "\033\033\033e/test2.rb?hello=Simon"
# $stdout.flush
# puts gets.inspect
# $stdout.flush
# puts gets.inspect
# $stdout.flush
# puts "byeW"
# $stdout.flush

puts "helloW"
system "capr-exec /test2.rb?hello=Simon"
puts "byeW"
