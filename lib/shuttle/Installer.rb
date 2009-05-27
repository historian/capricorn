
module Shuttle
  class Installer
    
    def install
      Shuttle::Domain.all.each do |domain|
        if domain.needs_setup?
          install_domain(domain)
          domain.needs_setup! false
        end
      end
    end
    
    def install_domain(domain)
      # before install
      
      # after install
    end
    
  end
end