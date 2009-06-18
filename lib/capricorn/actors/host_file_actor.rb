
module Capricorn
  module Actors # :nodoc:
    class HostFileActor < Capricorn::Actor
      
      on_install_satellite   :add_host_name
      on_uninstall_satellite :remove_host_name
      
      # addthe fost form the host file
      def add_host_name
        content = load_file
        content[:capricorn]['127.0.0.1'] ||= []
        content[:capricorn]['127.0.0.1'].push satellite.domain
        dump_file(content)
      end
      
      # remove the fost form the host file
      def remove_host_name
        content = load_file
        content[:capricorn]['127.0.0.1'] ||= []
        content[:capricorn]['127.0.0.1'].delete satellite.domain
        dump_file(content)
      end
      
      SEPERATOR = '### the content below this line is managed by capricorn'
      
    private
      
      def load_file
        content = File.read(system.host_file_path)
        content_parts = content.split(Capricorn::Actors::HostFileActor::SEPERATOR)
        
        user_hosts = content_parts[0] || ''
        host_lines = content_parts[1] || ''
        host_lines = host_lines.split(/\n/)
        host_lines = host_lines.inject({}) do |m, host_line|
          next if host_line.nil?
          host_line = host_line.strip
          next if host_line[0,1] == '#'
          next if host_line      == ''
          
          host_line = host_line.split(/\s+/)
          ip = host_line.shift
          m[ip] ||= []
          m[ip].concat(host_line)
          
          m
        end
        
        {
          :user      => user_hosts,
          :capricorn => host_lines
        }
      end
      
      def dump_file(content)
        File.open(system.host_file_path, 'w+') do |f|
          f.puts content[:user].strip
          f.puts Capricorn::Actors::HostFileActor::SEPERATOR
          content[:capricorn].each do |ip, hosts|
            f.puts "#{ip} #{hosts.join(' ')}"
          end
        end
      end
      
      module Config
        
        # path to the hosts file.
        def host_file_path(&block)
          option(:host_file_path, block) { |v| v or '/etc/hosts' }
        end
        
      end
      
    end
  end
end
