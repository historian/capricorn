
module Capricorn
  class System
    module Satellites
      
      def satellites_hash
        @satellites || load_satellites
      end
      
      def satellites
        satellites_hash.values
      end
      
      def find_satellite(domain)
        satellites_hash[domain]
      end
      
      def save_satellite!(satellite)
        satellites_hash[satellite.domain] = satellite
        satellite.dump_file(self.path('satellites', "#{satellite.domain}.yml"))
      end
      
      def destroy_satellite!(satellite)
        satellites_hash.delete(satellite.domain)
        FileUtils.rm_f(self.path('satellites', "#{satellite.domain}.yml"))
      end
      
      private
      
      def load_satellites
        @satellites = {}
        
        FileUtils.mkdir_p(self.path('satellites'))
        Dir.glob(self.path('satellites', '*.yml')).each do |yml|
          satellite = Capricorn::Satellite.load_file(yml)
          @satellites[satellite.domain] = satellite
        end
        
        @satellites
      end
      
    end
  end
end
