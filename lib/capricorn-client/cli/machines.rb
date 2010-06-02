class Capricorn::CLI::Machines < Thor
  include Capricorn::Helpers

  desc "list", "list all machines"
  def list
    ui.table("Machines", self.machines)
  end

  desc "reboot [NODE]", "reboot a node"
  def reboot(node=nil)
    node ||= select_node
    puts client.call.runtime.reboot(node.to_sym)
  end

  desc "selfupdate [NODE]", "update a node"
  def selfupdate(node=nil)
    node ||= select_node
    puts client.call.runtime.selfupdate(node.to_sym)
  end

end