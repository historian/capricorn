
module Shuttle
  class Linker
    
    def link
      Shuttle::Domain.all.each do |domain|
        if domain.needs_link?
          link_domain(domain)
          domain.needs_link! false
        end
      end
    end
    
    def link_domain(domain)
      # before link
      
      # after link
    end
    
  end
end