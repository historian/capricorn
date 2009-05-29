
module Shuttle
  module Adapters # :nodoc:
    class PleskAdapter < Shuttle::Adapter
      
      on_install   :create_client
      on_install   :create_domain
      on_install   :create_database
      on_uninstall :destroy_domain
      
      def create_client
        # if ./client --info JDoe
          # ./client --create JDoe -name "John Doe" -passwd sample -country US -notify false
      end
      
      def create_database
        # ./database --create db_name -domain basedomain -server localhost -print-id -type mysql -add_user db_user -passwd secret
      end
      
      def create_domain
        # ./domain --create example.com -clogin JDoe -status enabled -hosting true -hst_type phys -dns true -www true -ip VPS-IP
        
        # ./subdomain --create subdomain -domain example.com
      end
      
      def destroy_domain
        # ./domain --remove example.com
        
        # ./subdomain --remove -subdomains forum -domain example.com
      end
      
    end
  end
end
