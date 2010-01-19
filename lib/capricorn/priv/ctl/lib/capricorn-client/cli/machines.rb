class Capricorn::CLI::Machines < Thor
  include Capricorn::Helpers
  
  desc "list", "list all machines"
  def list
    ui.table("Machines", self.machines)
  end
  
end