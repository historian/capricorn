Dir.glob(File.dirname(File.expand_path(__FILE__))+'/vendor/*/lib').each do |lib|
  $:.unshift lib
end