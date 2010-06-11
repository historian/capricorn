class Capricorn::CLI::Machines < Capricorn::CLI
  include Capricorn::Helpers

  namespace :machines

  desc "list", "list all machines"
  def list
    machines.sort! do |a, b|
      a <=> b
    end

    puts "Machines:"
    machines.each do |machine|
      puts "- #{machine}"
    end
  end

  desc "reboot", "reboot a node"
  method_option :node, :type => :string
  def reboot
    puts client.call.runtime.reboot(node.to_sym)
  end

  desc "selfupdate", "update a node"
  method_option :node, :type => :string
  def selfupdate
    puts client.call.runtime.selfupdate(node.to_sym)
  end

end