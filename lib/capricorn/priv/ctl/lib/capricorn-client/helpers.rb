module Capricorn::Helpers
  
  def select_machine
    if self.machines.size > 1
      ui.choose(*self.machines)
    elsif self.machines.size == 1 and ui.agree("Use machine #{self.machines.first}? ")
      self.machines.first
    else
      puts ui.color('No machines found!!!', :red)
      exit(1)
    end
  end
  
  def select_application
    machine = select_machine
    
    apps = client.call.applications.all(machine.to_sym).last || []
    apps.collect! do |app|
      app[1]
    end
    apps.flatten!
    
    if apps.size > 1
      [machine, ui.choose(*apps)]
    elsif apps.size == 1 and ui.agree("Use application #{apps.first}? ")
      [machine, apps.first]
    else
      puts ui.color('No application found!!!', :red)
      exit(1)
    end
  end
  
  def machines
    @machines ||= (client.call.machines.all || []).collect do |machine|
      machine.to_s
    end
  end
  
  def client
    @client ||= BERTRPC::Service.new('localhost', 3457)
  end
  
  def ui
    @ui ||= begin
      @ui = HighLine.new
      @ui.extend UI
      @ui
    end
  end
  
  module UI
    
    def table(headers, items, mode=:rows, option=nil)
      headers = [headers].flatten.collect { |h| color(h, :green) }
      @output.puts list(headers+items, mode, option)
    end
    
  end
end